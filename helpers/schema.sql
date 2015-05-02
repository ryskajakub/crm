create table photos (
  id serial ,
  data bytea );

create table photos_meta (
  photo_id integer ,
  mime_type varchar (500) ,
  file_name varchar (500) );

create table machine_photos (
  photo_id integer ,
  machine_id integer );

create table companies (
  id serial , 
  name varchar(500) , 
  plant varchar(500) ,
  address varchar (500) ,
  person varchar (500) ,
  phone varchar (500) );

create table machine_types (
  id serial , 
  name varchar(500) , 
  manufacturer varchar (500) );

create table machines (
  id serial , 
  company_id integer , 
  contact_person_id integer , 
  machine_type_id integer , 
  operation_start date ,
  initial_mileage integer ,
  mileage_per_year integer ,
  note varchar (500) ,
  serial_number varchar (500) ,
  year_of_manufacture varchar (500) );

create table upkeeps (
  id serial ,
  date_ date , 
  closed boolean ,
  employee_id integer ,
  work_hours varchar (500) ,
  work_description varchar (5000) ,
  recommendation varchar (5000) );

create table upkeep_machines (
  upkeep_id integer ,
  note varchar (5000) ,
  machine_id integer ,
  recorded_mileage integer ,
  warranty boolean );

create table employees (
  id serial ,
  name varchar (500) ,
  contact varchar (500) ,
  capabilities varchar (500) );

create table upkeep_sequences (
  display_ordering integer ,
  label varchar (500) ,
  repetition integer ,
  machine_type_id integer ,
  one_time boolean );

create table contact_persons (
  id serial ,
  company_id integer ,
  name varchar (500) ,
  phone varchar (500) ,
  position varchar (500) )
