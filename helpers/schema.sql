create table password (
  password bytea not null );

create table photos (
  id serial primary key ,
  data bytea not null );

create table photos_meta (
  photo_id integer primary key references photos (id) ,
  mime_type varchar (500) not null ,
  file_name varchar (500) not null );

create table companies (
  id serial primary key , 
  name varchar(500) not null , 
  note varchar(500) not null ,
  address varchar (500) not null ,
  latitude float8 ,
  longitude float8 ,
  unique (name) );

create table machine_types (
  id serial primary key , 
  machine_kind integer not null ,
  name varchar(500) unique not null , 
  manufacturer varchar (500) );

create table contact_persons (
  id serial primary key ,
  company_id integer references companies (id) not null ,
  name varchar (500) not null ,
  phone varchar (500) not null ,
  position varchar (500) not null );

create table machines (
  id serial primary key ,
  company_id integer references companies (id) not null ,
  contact_person_id integer references contact_persons (id) ,
  machine_type_id integer references machine_types (id) not null ,
  linkage_id integer references machines (id) ,
  operation_start date ,
  initial_mileage integer not null ,
  mileage_per_year integer not null ,
  note varchar (500) not null ,
  serial_number varchar (500) not null ,
  year_of_manufacture varchar (500) not null ,
  archived boolean not null ,
  note varchar (5000) not null ,
  upkeep_by integer not null );

create table machine_photos (
  photo_id integer references photos (id) ,
  machine_id integer references machines (id) ,
  primary key (photo_id, machine_id) );

create table upkeep_photos (
  photo_id integer references photos (id) ,
  upkeep_id integer references upkeeps (id) ,
  primary key (photo_id, upkeep_id) );

create table employees (
  id serial primary key ,
  name varchar (500) unique not null ,
  contact varchar (500) not null ,
  capabilities varchar (500) not null ,
  colour varchar (500) not null );

create table upkeeps (
  id serial primary key ,
  date_ date not null , 
  closed boolean not null ,
  work_hours varchar (500) not null ,
  work_description varchar (5000) not null ,
  recommendation varchar (5000) not null ,
  set_date boolean not null );

create table upkeep_machines (
  upkeep_id integer not null references upkeeps (id) ,
  note varchar (5000) not null ,
  machine_id integer not null references machines (id) ,
  recorded_mileage integer not null ,
  warranty boolean not null ,
  end_note varchar (5000) not null ,
  upkeep_type smallint not null ,
  primary key (upkeep_id, machine_id) );

create table upkeep_sequences (
  display_ordering integer ,
  label varchar (500) not null ,
  repetition integer not null ,
  machine_type_id integer references machine_types (id) ,
  one_time boolean not null ,
  primary key (display_ordering, machine_type_id) );

create table extra_field_settings (
  id serial primary key ,
  kind integer not null ,
  order_ integer not null ,
  name varchar (500) not null );

create table extra_fields (
  extra_field_id integer not null references extra_field_settings (id) ,
  machine_id integer not null references machines (id) ,
  value varchar (5000) not null ,
  primary key (extra_field_id, machine_id) );

create table upkeep_employees (
  upkeep_id integer not null references upkeeps (id) ,
  employee_id integer not null references employees (id) ,
  order_ integer not null );

create table task_employees (
  task_id integer not null references tasks (id) ,
  employee_id integer not null references employees (id));

create table tasks (
  id serial primary key ,
  start_date date not null ,
  end_date date ,
  description varchar (5000) not null );
