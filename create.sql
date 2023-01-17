create table if not exists dns_data(
  id serial,
  created timestamp default now(),
  domain_name varchar(512) not null,
  qtype varchar(16) not null,
  rtype varchar(16) not null,
  response varchar(256),
  dns inet
);

create table if not exists dns_error(
  id serial,
  created timestamp default now(),
  domain_name varchar(512) not null,
  qtype varchar(16) not null,
  rerror varchar(128) not null,
  dns inet
);
