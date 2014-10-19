create table Company (
  id int not null auto_increment
  , name varchar(255) not null
  , days int not null
  , active bool not null
  , primary key(id)
  , unique(name)
);
