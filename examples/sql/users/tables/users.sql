create table users (
  user_id     bigint    primary key
, name        varchar2  not null
, age         bigint    not null
, is_active   boolean   not null default true);
