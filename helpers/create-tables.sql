drop table companies;
create table companies (
  id serial
  , name varchar(500)
  , plant varchar(500)
);

drop table machine_types;
create table machine_types (
  id serial
  , name varchar(500)
  , manufacturer varchar (500)
);

drop table machines;
create table machines (
  id serial
  , company_id integer
  , machine_type_id integer
  , operation_start varchar (500)
);

drop table upkeeps;
create table upkeeps (
  id serial ,
  date_ varchar (500) );

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

insert into machine_types(name, manufacturer) values ('BK 150', 'Remeza');
insert into machine_types(name, manufacturer) values ('BK 75', 'Remeza');
insert into machine_types(name, manufacturer) values ('EK 4', 'Orlík');

insert into machines(company_id, machine_type_id, operation_start)
  values (1, 1, '1999');
insert into machines(company_id, machine_type_id, operation_start)
  values (1, 2, '1999');
insert into machines(company_id, machine_type_id, operation_start)
  values (1, 3, '2007');
insert into machines(company_id, machine_type_id, operation_start)
  values (2, 1, '2001');
insert into machines(company_id, machine_type_id, operation_start)
  values (2, 3, '2008');
