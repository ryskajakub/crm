select json_build_object('warranty', warranty, 'noFaults', no_faults, 'km', CASE WHEN transport is null THEN 0 ELSE transport END, 'transport', CASE WHEN transport IS NULL THEN 'reality' ELSE 'km' END, 'jobs', ufj.jobs, 'parts', ufp.parts) as form from upkeep_forms as uf
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
where uf.upkeep_id = 1283
