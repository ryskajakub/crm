import express from "express";
import bodyParser from "body-parser";

import crypto from "crypto";

import h from "react-hyperscript";
import * as ReactDOMServer from "react-dom/server";

import fs from "fs";

import { fromBuffer } from "pdf2pic";

// @ts-ignore
import pg from "pg";
import { App } from "./App.mjs";
import { DateTime } from "luxon";

import puppeteer from "puppeteer";

import Mailgun from "mailgun.js";
import formData from "form-data";

const { Client } = pg;

const app = express();
const port = 8001;

const clientConfig = {
  user: "haskell",
  database: "crm",
  host: "localhost",
  password: "haskell",
};

app.use(bodyParser.json());

/**
 * @param {Buffer} pdf
 * @param {any} client
 * @param {number} upkeepId
 * @returns { Promise<void> }
 */
async function savePicture(pdf, client, upkeepId) {
  const random = crypto.randomBytes(16).toString("hex");
  const name = `${random}`;
  await fromBuffer(pdf, {
    quality: 1000,
    density: 1000,
    width: 1140,
    height: 1140 * 1.4142857142857144,
    format: "jpeg",
    saveFilename: name,
    savePath: "/tmp",
  })(1, false);

  const imgData = await fs.promises.readFile(`/tmp/${name}.1.jpeg`, "hex");
  const allImgData = "\\x" + imgData;
  const result = await client.query(
    `insert into photos(data) values ($1) returning id`,
    [allImgData]
  );
  /** @type { {id: number}[] } */
  const rows = result.rows;
  const returnedId = rows[0].id;
  await client.query(
    "insert into photos_meta(photo_id, mime_type, file_name) values($1, $2, $3)",
    [returnedId, "image/jpeg", "image.jpg"]
  );
  await client.query(
    "insert into upkeep_photos(photo_id, upkeep_id)values ($1, $2)",
    [returnedId, upkeepId]
  );
}

/**
 * @param {Buffer} pdf
 * @param {string} to
 * @returns { Promise<void> }
 */
async function sendMails(pdf, to) {
  const mailgun = new Mailgun(formData);
  const mg = mailgun.client({
    username: "api",
    key: process.env.MAILGUN_API_KEY || "",
    url: "https://api.mailgun.net",
  });

  await mg.messages.create("2e.cz", {
    from: "info@2e.cz",
    to,
    // cc: "ryska@2e.cz",
    subject: `Servisní list 2e`,
    text: `Dobrý den, 

posíláme v příloze servisní list.

S pozdravem
2e.cz
`,
    attachment: [
      {
        data: pdf,
        filename: "servisni-list-2e.pdf",
      },
    ],
  });

  console.log(`sent message to ryskajakub@seznam.cz`);
}

/**
 * @param {number} upkeepId
 * @param {any} client
 * @returns { Promise<Buffer> }
 */
async function generatePdf(upkeepId, client) {
  const datas = await getAllData(upkeepId, client);
  const data = datas[0];

  if (datas[0] && data.form && data.signatures) {
    /** @type { import("./Data.t").AppPropsDataServer } */
    const appPropsData = {
      type: "server",
      parsedForm: data.form,
      signatures: data.signatures,
      upkeep: {
        ...data.upkeep,
        date: DateTime.fromISO(data.upkeep.date),
      },
      upkeepId,
    };

    const reactApp = h(App, { data: appPropsData, signature: true });

    const body = ReactDOMServer.renderToString(reactApp);

    const style = (
      await fs.promises.readFile(
        `/app/node_modules/bootstrap/dist/css/bootstrap.min.css`
      )
    ).toString();

    const html = `
<html lang="en">
  <head>
  <style>
    ${style}
  </style>
  </head>
  <body>
    ${body}
  </body>
</html>
    `;

    let args = ["--no-sandbox", "--disable-setuid-sandbox"];

    const browser = await puppeteer.launch({
      args: args,
    });
    const page = await browser.newPage();
    await page.setContent(html, { waitUntil: ["networkidle0", "load"] });

    const pdfData = await page.pdf({ scale: 0.77 });
    await browser.close();

    const buffer = Buffer.from(Object.values(pdfData));

    return buffer;
  } else {
    throw new Error("db");
  }
}

const q1 = `select uf.upkeep_id, json_build_object('warranty', warranty, 'noFaults', no_faults, 'km', CASE WHEN transport is null THEN 0 ELSE transport END, 'transport', CASE WHEN transport IS NULL THEN 'reality' ELSE 'km' END, 'jobs', ufj.jobs, 'parts', ufp.parts) as form from upkeep_forms as uf
join (
    select upkeep_id,
    json_agg(json_build_object('date', date_, 'travelThereFrom', travel_there_from, 'travelThereTo', travel_there_to, 'workTo', work_to, 'workFrom', work_from, 'travelBackFrom', travel_back_from, 'travelBackTo', travel_back_to, 'note', note)) as jobs
    from upkeep_form_jobs
    group by upkeep_id
) as ufj on ufj.upkeep_id = uf.upkeep_id
join (
    select upkeep_id, json_agg(json_build_object('number', number, 'name', name, 'quantity', quantity, 'machine_id', machine_id)) as parts
    from upkeep_form_parts
    group by upkeep_id
) as ufp on ufp.upkeep_id = uf.upkeep_id
`;

/**
 * @param {number} upkeepId
 * @param {any} client
 * @returns { Promise<import("./Data.t").Db<import("./Data.t").DownloadData>[]> }
 */
async function getAllData(upkeepId, client) {
  try {
    const q = `select 
json_build_object('description', upkeeps.work_description, 'recommendation', upkeeps.recommendation, 'company_name', companies.name, 'employees', COALESCE(u.employees, json_build_array()), 'available_employees', (select json_agg(json_build_object('id', id, 'name', name)) as e from employees), 'machines', um.machines, 'date', upkeeps.date_) as upkeep,
jf.form,
ufs.signatures
from upkeeps 
left join (${q1}) as jf on jf.upkeep_id = upkeeps.id
left join (
  select upkeep_id, json_build_object('theirs', theirs, 'ours', ours) as signatures from upkeep_form_signatures
) as ufs on upkeeps.id = ufs.upkeep_id
join (
  select upkeep_id, max(company_id) as company_id from upkeep_machines join machines on machines.id = upkeep_machines.machine_id group by upkeep_id
) as company_ids on upkeeps.id = company_ids.upkeep_id
join companies on companies.id = company_ids.company_id
left join (
  select upkeep_id, json_agg(employees.id) as employees from upkeep_employees join employees on upkeep_employees.employee_id = employees.id group by upkeep_id
) as u on u.upkeep_id = upkeeps.id
join (
  select upkeep_id, json_agg(json_build_object('serial_number', machines.serial_number, 'mileage', upkeep_machines.recorded_mileage, 'type', machine_types.name, 'manufacturer', machine_types.manufacturer, 'machine_id', machines.id)) as machines
  from upkeep_machines
  join machines on machines.id = upkeep_machines.machine_id
  join machine_types on machines.machine_type_id = machine_types.id
  group by upkeep_machines.upkeep_id
) as um on um.upkeep_id = upkeeps.id
where upkeeps.id = $1
    `;
    const result = await client.query(q, [upkeepId]);
    /** @type { import("./Data.t").Db<import("./Data.t").DownloadData>[] } */
    const rows = result.rows;

    return rows;
  } catch (e) {
    console.log(e);
    return [];
  }
}

app.get("/tsapi/data/:id", async (req, res) => {
  const client = new Client(clientConfig);

  const id = Number(req.params.id);
  if (Number.isNaN(id)) {
    res.status(400).send();
  }

  try {
    await client.connect();
    const rows = await getAllData(id, client);

    if (rows.length === 1) {
      res.send(rows[0]);
    } else {
      res.status(404).send();
    }
  } catch (e) {
    console.log(e);
    res.status(500).send();
  } finally {
    return client.end();
  }
});

app.put("/tsapi/data/:id", async (req, res) => {
  const upkeepId = Number(req.params.id);

  if (Number.isNaN(upkeepId)) {
    res.status(400).send();
    return;
  }

  /** @type { import("./Data.t").Payload<import("./Data.t").FormState> } */
  const body = req.body;

  const client = new Client(clientConfig);
  try {
    await client.connect();
    await client.query(`BEGIN TRANSACTION`);
    await client.query(`delete from upkeep_employees where upkeep_id = $1`, [
      upkeepId,
    ]);
    await Promise.all(
      body.employees.map(async (employeeId, index) => {
        return await client.query(
          `insert into upkeep_employees(upkeep_id, employee_id, order_) values($1, $2, $3)`,
          [upkeepId, employeeId, index]
        );
      })
    );
    await Promise.all(
      Object.entries(body.mileages).map(
        async ([machine_id, mileage]) =>
          await client.query(
            `update upkeep_machines set recorded_mileage = $1 where machine_id = $2 and upkeep_id = $3`,
            [mileage, machine_id, upkeepId]
          )
      )
    );
    await client.query(
      `update upkeeps set work_description = $1, recommendation = $2 where id = $3`,
      [body.description, body.recommendation, upkeepId]
    );
    await client.query(`delete from upkeep_forms where upkeep_id = $1`, [
      upkeepId,
    ]);
    await client.query(
      `insert into upkeep_forms(upkeep_id, transport, warranty, no_faults) values($1, $2, $3, $4)`,
      [
        upkeepId,
        body.transport === "km" ? body.km : null,
        body.warranty,
        body.noFaults,
      ]
    );
    await client.query(`delete from upkeep_form_parts where upkeep_id = $1`, [
      upkeepId,
    ]);
    await Promise.all(
      body.parts.map(async (part) => {
        return await client.query(
          `insert into upkeep_form_parts(upkeep_id, number, name, quantity, machine_id) values ($1, $2, $3, $4, $5)`,
          [upkeepId, part.number, part.name, part.quantity, part.machine_id]
        );
      })
    );
    await client.query(`delete from upkeep_form_jobs where upkeep_id = $1`, [
      upkeepId,
    ]);
    await Promise.all(
      body.jobs.map(async (job) => {
        return await client.query(
          `insert into upkeep_form_jobs(upkeep_id, date_, travel_there_from, travel_there_to, work_from, work_to, travel_back_from, travel_back_to, note) values ($1, $2, $3, $4, $5, $6, $7, $8, $9)`,
          [
            upkeepId,
            new Date(job.date),
            job.travelThereFrom,
            job.travelThereTo,
            job.workFrom,
            job.workTo,
            job.travelBackFrom,
            job.travelBackTo,
            job.note,
          ]
        );
      })
    );
    await client.query("COMMIT");
    res.send();
  } catch (e) {
    await client.query("ROLLBACK");
    console.log(e);
    res.status(500).send();
  } finally {
    return client.end();
  }
});

app.put("/tsapi/signatures/:upkeepId", async (req, res) => {
  /** @type { import("./Data.t").Signatures } */
  const data = req.body;
  const upkeepId = Number(req.params.upkeepId);

  if (Number.isNaN(upkeepId)) {
    res.status(400).send();
    return;
  }

  const client = new Client(clientConfig);
  try {
    await client.connect();
    await client.query(`BEGIN TRANSACTION`);
    await client.query(`delete from upkeep_form_signatures`);
    await client.query(
      `insert into upkeep_form_signatures(upkeep_id, theirs, ours) values ($1, $2, $3)`,
      [upkeepId, data.theirs, data.ours]
    );
    await client.query(`insert into upkeep_form_mails(upkeep_id, email, sent_at) values ($1, $2, $3)`, [upkeepId, data.email, new Date()])
    const pdf = await generatePdf(upkeepId, client);
    await savePicture(pdf, client, upkeepId);
    await sendMails(pdf, data.email);
    await client.query("COMMIT");
    res.status(204).send();
  } catch (e) {
    await client.query("ROLLBACK");
    console.log(e);
    res.status(500).send();
  } finally {
    return client.end();
  }
});

app.post(`/tsapi/abc`, async (req, res) => {
  const client = new Client(clientConfig);
  try {
    client.connect();
    const pdf = await generatePdf(3737, client);
    await sendMails(pdf, "", client);
    // await savePicture(pdf, client, 3737);
  } catch (e) {
    console.log(e);
  } finally {
    res.send();
    client.end();
  }
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`);
});
