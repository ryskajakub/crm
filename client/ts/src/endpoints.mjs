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

app.get("/tsapi/data/:id", async (req, res) => {
  const client = new Client(clientConfig);
  try {
    await client.connect();
    const upkeepId = req.params.id;
    const result = await client.query(
      `
select 
  companies.name as company_name,
  u.employees,
  (select json_agg(json_build_object('id', id, 'name', name)) as e from employees) as available_employees,
  um.machines,
  upkeeps.date_ as date
from upkeeps 
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
    `,
      [upkeepId]
    );
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
  const upkeepId = req.params.id;
  /** @type { import("./Data.t").Payload<import("./Data.t").ParsedForm> } */
  const rawBody = req.body;

  /** @type { import("./Data.t").ParsedForm } */
  const body = {
    ...rawBody,
    jobs: rawBody.jobs.map((j) => ({
      ...j,
      date: new Date(j.date),
      travelThereFrom: new Date(j.travelThereFrom),
      travelThereTo: new Date(j.travelThereTo),
      workFrom: new Date(j.workFrom),
      workTo: new Date(j.workTo),
      travelBackFrom: new Date(j.travelBackFrom),
      travelBackTo: new Date(j.travelBackTo),
    })),
  };

  const client = new Client(clientConfig);
  try {
    await client.connect();
    client.query(`delete from upkeep_employees where upkeep_id = $1`, [
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
    await client.query(
      `update upkeeps set work_description = $1, recommendation = $2 where upkeep_id = $3`,
      [body.description, body.recommendation, upkeepId]
    );
    await client.query(
      `insert into upkeep_forms(upkeep_id, transport, warranty, no_faults) values($1, $2, $3, $4)`,
      [
        upkeepId,
        body.transport === "km" ? body.km : null,
        body.warranty,
        body.noFaults,
      ]
    );
    await Promise.all(
      body.parts.map(async (part) => {
        return await client.query(
          `insert into upkeep_parts(upkeep_id, number, name, quantity, machine_id) values ($1, $2, $3, $4, $5)`,
          [upkeepId, part.number, part.name, part.quantity, part.machine_id]
        );
      })
    );
    await Promise.all(
      body.jobs.map(async (job) => {
        return await client.query(
          `insert into upkeep_jobs(upkeep_id, date_, travel_there_from, travel_there_to, work_from, work_to, travel_back_from, travel_back_to, note) values ($1, $2, $3, $4, $5, $6, $7, $8, $9)`,
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
  } catch (e) {
    console.log(e);
    res.status(500).send();
  } finally {
    return client.end();
  }
});

app.put("/tsapi/upload/:upkeepId", async (req, res) => {
  /** @type { import("./Data.t").Signatures } */
  const data = req.body;
  const upkeepId = req.params.upkeepId;

  const client = new Client(clientConfig);
  try {
    await client.connect();
    client.query(
      `insert into upkeep_signatures(upkeep_id, theirs, ours) values ($1, $2, $3)`,
      [upkeepId, data.theirs, data.ours]
    );
  } catch (e) {
    console.log(e);
    res.status(500).send();
  } finally {
    return client.end();
  }

  // @ts-ignore
  res.send();
});

app.get;

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`);
});
