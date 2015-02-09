drop table photos;
create table photos (
  id serial ,
  data bytea );

drop table photos_meta;
create table photos_meta (
  photo_id integer ,
  mime_type varchar (500) ,
  file_name varchar (500) );

drop table machine_photos;
create table machine_photos (
  photo_id integer ,
  machine_id integer );

drop table companies;
create table companies (
  id serial , 
  name varchar(500) , 
  plant varchar(500) ,
  address varchar (500) ,
  person varchar (500) ,
  phone varchar (500) );

drop table machine_types;
create table machine_types (
  id serial , 
  name varchar(500) , 
  manufacturer varchar (500) );

drop table machines;
create table machines (
  id serial , 
  company_id integer , 
  machine_type_id integer , 
  operation_start date ,
  initial_mileage integer ,
  mileage_per_year integer ,
  note varchar (500) );

drop table upkeeps;
create table upkeeps (
  id serial ,
  date_ date , 
  closed boolean ,
  employee_id integer ,
  work_hours varchar (500) ,
  work_description varchar (5000) ,
  recommendation varchar (5000));

drop table upkeep_machines;
create table upkeep_machines (
  upkeep_id integer ,
  note varchar (500) ,
  machine_id integer ,
  recorded_mileage integer );

drop table employees;
create table employees (
  id serial ,
  name varchar (500) );

drop table upkeep_sequences;
create table upkeep_sequences (
  display_ordering integer ,
  label varchar (500) ,
  repetition integer ,
  machine_type_id integer ,
  one_time boolean );


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
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year, note)
  values (1, 1, '1999-01-01', 0, 365 * 24, '');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year, note)
  values (1, 2, '1999-01-01', 10000, 365 * 24, '');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year, note)
  values (1, 3, '2007-01-01', 0, 365 * 8, '');

insert into companies(name, plant, address, person, phone) values ('České dráhy', 'I', 'Ostrava', 'Zbieczuk', '777 888 222');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year, note)
  values (2, 1, '2001-01-01', 30000, 365 * 12, '');
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year, note)
  values (2, 3, '2008-01-01', 0, 365 * 24, '');


insert into companies(name, plant, address, person, phone) values ('FOMA Bohemia', 'Ústředna', 'Praha', 'Loučka', '777 111 111');
insert into companies(name, plant, address, person, phone) values ('FOMA Bohemia', 'Provozovna', 'Vodňany', 'Mysliveček', '335 881 233');


insert into upkeeps(date_, closed, employee_id, work_hours, work_description, recommendation)
  values ('1999-01-01', FALSE, null, '0', '', '');
insert into upkeeps(date_, closed, employee_id, work_hours, work_description, recommendation)
  values ('2001-01-01', FALSE, null, '0', '', '');
insert into upkeeps(date_, closed, employee_id, work_hours, work_description, recommendation)
  values ('2008-01-01', FALSE, 1, '0', '', '');

insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage) values (1, 'oprava', 1, 0);
insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage) values (1, 'pravidelný', 2, 0);
insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage) values (2, 'oprava 2', 2, 0);
insert into upkeep_machines(upkeep_id, note, machine_id, recorded_mileage) values (3, 'údržba', 4, 0);


insert into employees(name) values ('Kutička');
insert into employees(name) values ('Mandlík');
