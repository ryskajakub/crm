create table photos (
  id serial primary key ,
  data bytea );

create table photos_meta (
  photo_id integer primary key references photos (id) ,
  mime_type varchar (500) ,
  file_name varchar (500) );

create table companies (
  id serial primary key , 
  name varchar(500) , 
  plant varchar(500) ,
  address varchar (500) ,
  person varchar (500) ,
  phone varchar (500) ,
  unique (name, plant) );

create table machine_types (
  id serial primary key , 
  name varchar(500) unique , 
  manufacturer varchar (500) );

create table contact_persons (
  id serial primary key ,
  company_id integer references companies (id) ,
  name varchar (500) ,
  phone varchar (500) ,
  position varchar (500) );

create table machines (
  id serial primary key ,
  company_id integer references companies (id) ,
  contact_person_id integer references contact_persons (id) ,
  machine_type_id integer references machine_types (id) ,
  operation_start date ,
  initial_mileage integer ,
  mileage_per_year integer ,
  note varchar (500) ,
  serial_number varchar (500) ,
  year_of_manufacture varchar (500) );

create table machine_photos (
  photo_id integer references photos_meta (photo_id) ,
  machine_id integer references machines (id) ,
  primary key (photo_id, machine_id) );

create table employees (
  id serial primary key ,
  name varchar (500) unique ,
  contact varchar (500) ,
  capabilities varchar (500) );

create table upkeeps (
  id serial primary key ,
  date_ date , 
  closed boolean ,
  employee_id integer references employees (id) ,
  work_hours varchar (500) ,
  work_description varchar (5000) ,
  recommendation varchar (5000) );

create table upkeep_machines (
  upkeep_id integer references upkeeps (id) ,
  note varchar (5000) ,
  machine_id integer references machines (id) ,
  recorded_mileage integer ,
  warranty boolean ,
  primary key (upkeep_id, machine_id) );

create table upkeep_sequences (
  display_ordering integer ,
  label varchar (500) ,
  repetition integer ,
  machine_type_id integer references machine_types (id) ,
  one_time boolean ,
  primary key (display_ordering, machine_type_id) );
