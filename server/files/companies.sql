drop table companies;

create table companies (
  id serial
  , name varchar(500)
  , plant varchar(500)
);

insert into companies(name, plant) values ('Continental', 'I');
insert into companies(name, plant) values ('České dráhy', 'I');
insert into companies(name, plant) values ('FOMA Bohemia', 'I');

/*
  companyBase = Company 0 "Continental" "I" "p.Jelínek" "721 650 194" "Brandýs nad labem"
  companyNames = ["Continental","České dráhy","FOMA Bohemia","Kand","Metrostav","Neumann","PREX","Stachema Kolín","Valsabbia"]
*/
