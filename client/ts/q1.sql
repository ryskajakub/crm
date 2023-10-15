select 
json_build_object('description', upkeeps.work_description, 'recommendation', upkeeps.recommendation, 'company_name', companies.name, 'employees', u.employees, 'available_employees', (select json_agg(json_build_object('id', id, 'name', name)) as e from employees), 'machines', um.machines, 'date', upkeeps.date_) as upkeep,
jf.form
from upkeeps 
left join (select uf.upkeep_id, json_build_object('warranty', warranty, 'noFaults', no_faults, 'km', CASE WHEN transport is null THEN 0 ELSE transport END, 'transport', CASE WHEN transport IS NULL THEN 'reality' ELSE 'km' END, 'jobs', ufj.jobs, 'parts', ufp.parts) as form from upkeep_forms as uf
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
) as jf on jf.upkeep_id = upkeeps.id
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
where upkeeps.id = 1283
