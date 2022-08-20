import express from "express";
import bodyParser from "body-parser";

// @ts-ignore
import pg from "pg";

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

app.get("/tsapi/data/:id", async (req, res) => {
  const client = new Client(clientConfig);
  try {
    await client.connect();
    const upkeepId = req.params.id;
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
    const rows = result.rows;
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
  const rawBody = req.body;

  /** @type { import("./Data.t").Db<import("./Data.t").FormState> } */
  const body = {
    ...rawBody,
    jobs: rawBody.jobs.map((j) => {
      return {
        ...j,
        date: new Date(j.date),
      };
    }),
  };

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
            job.date,
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
  const upkeepId = req.params.upkeepId;

  const client = new Client(clientConfig);
  try {
    await client.connect();
    await client.query(`delete from upkeep_form_signatures`);
    await client.query(
      `insert into upkeep_form_signatures(upkeep_id, theirs, ours) values ($1, $2, $3)`,
      [upkeepId, data.theirs, data.ours]
    );
    res.status(204).send();
  } catch (e) {
    console.log(e);
    res.status(500).send();
  } finally {
    return client.end();
  }
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`);
});
