import express from "express";

// @ts-ignore
import pg from "pg";

const { Client } = pg;

const app = express();
const port = 8001;

app.get("/tsapi", async (req, res) => {
  const client = new Client({
    user: "haskell",
    database: "crm",
    host: "localhost",
    password: "haskell",
  });
  try {
    await client.connect();
    const upkeepId = req.query["id"];
    const result = await client.query(`
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
    `, [
      upkeepId,
    ]);
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

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`);
});
