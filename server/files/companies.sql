drop table companies;
create table companies (
  id serial
  , name varchar(500)
  , plant varchar(500)
);

drop table machines;
create table machines (
  company_id integer
  , name varchar(500)
);

with companies as (
  insert into companies(name, plant) values ('Continental', 'I') returning id
), machines as (
  select 'Machina 1' as name
  union
  select 'Albert abc' as name
)
insert into machines (company_id, name)
select id, name
from companies
cross join machines;

with companies as (
  insert into companies(name, plant) values ('České dráhy', 'II') returning id
), machines as (
  select 'Kompresor 1' as name
  union
  select 'Manesman demagog' as name
  union
  select 'Atlas copco XXX1' as name
)
insert into machines (company_id, name)
select id, name
from companies
cross join machines;

insert into companies(name, plant) values ('FOMA Bohemia', 'I');
insert into companies(name, plant) values ('Kand', 'I');
insert into companies(name, plant) values ('Metrostav', 'I');
insert into companies(name, plant) values ('Neumann', 'I');
insert into companies(name, plant) values ('PREX', 'I');
