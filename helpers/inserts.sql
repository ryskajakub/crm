insert into machine_types(name, manufacturer) values ('BK 150', 'Remeza');
insert into upkeep_sequences(display_ordering, label, repetition, machine_type_id, one_time) 
  values (1, '4000 mth', 5000, 1, FALSE);
insert into upkeep_sequences(display_ordering, label, repetition, machine_type_id, one_time) 
  values (2, 'Generální', 30000, 1, FALSE);

insert into machine_types(name, manufacturer) values ('BK 75', 'Remeza');
insert into upkeep_sequences(display_ordering, label, repetition, machine_type_id, one_time) 
  values (1, 'Běžný', 2000, 2, FALSE);

insert into machine_types(name, manufacturer) values ('EK 4', 'Orlík');
insert into upkeep_sequences(display_ordering, label, repetition, machine_type_id, one_time) 
  values (1, 'Běžný', 2000, 3, FALSE);


insert into companies(name, plant, address, person, phone) values ('Continental', 'I', 'Kolín', 'Novák', '777 123 456');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, 
  mileage_per_year, note, serial_number, year_of_manufacture)
    values (1, 1, '1999-01-01', 0, 365 * 24, '', 'b5789f8f1125c5d673ebb8a89c22b836', '1999');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, 
  mileage_per_year, note, serial_number, year_of_manufacture)
    values (1, 2, '1999-01-01', 10000, 365 * 24, '', 'b5789f8f1125c5d673ebb8a89c22b836', '1999');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, 
  mileage_per_year, note, serial_number, year_of_manufacture)
    values (1, 3, '2007-01-01', 0, 365 * 8, '', 'b5789f8f1125c5d673ebb8a89c22b836', '1999');

insert into companies(name, plant, address, person, phone) values ('České dráhy', 'I', 'Ostrava', 'Zbieczuk', '777 888 222');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, 
  mileage_per_year, note, serial_number, year_of_manufacture)
    values (2, 1, '2001-01-01', 30000, 365 * 12, '', 'b5789f8f1125c5d673ebb8a89c22b836', '1999');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, 
  mileage_per_year, note, serial_number, year_of_manufacture)
    values (2, 3, '2008-01-01', 0, 365 * 24, '', 'b5789f8f1125c5d673ebb8a89c22b836', '1999');


insert into companies(name, plant, address, person, phone) values ('FOMA Bohemia', 'Ústředna', 'Praha', 'Loučka', '777 111 111');
insert into companies(name, plant, address, person, phone) values ('FOMA Bohemia', 'Provozovna', 'Vodňany', 'Mysliveček', '335 881 233');


insert into upkeeps(date_, closed, employee_id, work_hours, work_description, recommendation)
  values ('1999-01-01', FALSE, null, '0', '', '');
insert into upkeeps(date_, closed, employee_id, work_hours, work_description, recommendation)
  values ('2001-01-01', FALSE, null, '0', '', '');
insert into upkeeps(date_, closed, employee_id, work_hours, work_description, recommendation)
  values ('2008-01-01', FALSE, 1, '0', '', '');

insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage, warranty)
  values (1, 'oprava', 1, 0, FALSE);
insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage, warranty)
  values (1, 'pravidelný', 2, 0, FALSE);
insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage, warranty)
  values (2, 'oprava 2', 2, 0, FALSE);
insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage, warranty)
  values (3, 'údržba', 4, 0, FALSE);


insert into employees(name) values ('Kutička');
insert into employees(name) values ('Mandlík');
