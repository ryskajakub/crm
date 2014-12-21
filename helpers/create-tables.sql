drop table companies;
create table companies (
  id serial
  , name varchar(500)
  , plant varchar(500)
);

drop table machine_types;
create table machine_types (
  id serial , 
  name varchar(500) , 
  manufacturer varchar (500) ,
  upkeep_per_mileage integer );

drop table machines;
create table machines (
  id serial , 
  company_id integer , 
  machine_type_id integer , 
  operation_start date ,
  initial_mileage integer ,
  mileage_per_year integer );

drop table upkeeps;
create table upkeeps (
  id serial ,
  date_ date );

drop table upkeep_machines;
create table upkeep_machines (
  upkeep_id integer ,
  note varchar (500) ,
  machine_id integer );

insert into companies(name, plant) values ('Continental', 'I');
insert into companies(name, plant) values ('České dráhy', 'II');
insert into companies(name, plant) values ('FOMA Bohemia', 'I');
insert into companies(name, plant) values ('Kand', 'I');
insert into companies(name, plant) values ('Metrostav', 'I');
insert into companies(name, plant) values ('Neumann', 'I');
insert into companies(name, plant) values ('PREX', 'I');

insert into machine_types(name, manufacturer, upkeep_per_mileage) values ('BK 150', 'Remeza', 365 * 12);
insert into machine_types(name, manufacturer, upkeep_per_mileage) values ('BK 75', 'Remeza', 365 * 24);
insert into machine_types(name, manufacturer, upkeep_per_mileage) values ('EK 4', 'Orlík', 365 * 24 * 2);

insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year)
  values (1, 1, '1999-01-01', 0, 365 * 24);
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year)
  values (1, 2, '1999-01-01', 10000, 365 * 24);
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year)
  values (1, 3, '2007-01-01', 0, 365 * 8);
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year)
  values (2, 1, '2001-01-01', 30000, 365 * 12);
insert into machines(company_id, machine_type_id, operation_start, initial_mileage, mileage_per_year)
  values (2, 3, '2008-01-01', 0, 365 * 24);

insert into upkeeps(date_) values ('1999-01-01');
insert into upkeeps(date_) values ('2001-01-01');
insert into upkeeps(date_) values ('2008-01-01');

insert into upkeep_machines(upkeep_id, note, machine_id) values (1, 'oprava', 1);
insert into upkeep_machines(upkeep_id, note, machine_id) values (1, 'pravidelný', 2);
insert into upkeep_machines(upkeep_id, note, machine_id) values (2, 'oprava 2', 2);

insert into upkeep_machines(upkeep_id, note, machine_id) values (3, 'údržba', 4);
