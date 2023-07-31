create table if not exists dns_data(
  id serial not null,
  created timestamptz not null default now(),
  domain_name varchar(512) not null,
  qtype varchar(16) not null,
  rtype varchar(16) not null,
  response varchar(256),
  dns inet,
  unique(domain_name,qtype,rtype,response),
  primary key(id)
);

create table if not exists dns_error(
  id serial not null,
  created timestamptz default now(),
  domain_name varchar(512) not null,
  qtype varchar(16) not null,
  rerror varchar(128) not null,
  dns inet,
  unique(domain_name,qtype,rerror),
  primary key(id)
);

create table if not exists nameservers(
  id serial not null,
  ip inet not null,
  timeouts integer default 0,
  enabled boolean default true,
  unique(ip),
  primary key(id)
);

create table if not exists domains(
  id serial not null,
  addr varchar(256) not null,
  unique(addr),
  primary key(id)
);
