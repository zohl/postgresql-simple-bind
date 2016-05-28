create domain varchar2 as varchar
  default ''
  not null
  check (length(value) <= 4000);


create function nvl(p varchar2, alt varchar2) returns varchar2 as
$nvl$
  select (case p when '' then alt else p end);
$nvl$
language sql;


create table users (
  user_id     bigint    primary key
, name        varchar2  not null
, age         bigint    not null
, is_active   boolean   not null default true);


create type t_user as (
  user_id  bigint
, name     varchar2
, age      bigint);


create function get_users(p_filter varchar2 default '') returns setof t_user as
$get_users_1$
  select user_id, name, age
  from users
  where is_active = true
    and name like nvl(p_filter, name)
  order by user_id;
$get_users_1$
language sql
security definer;


create function add_user(p_name varchar2, p_age bigint) returns bigint as
$add_user$
  insert into users(user_id, name, age)
  values (
    (select coalesce(max(user_id), 0) + 1 from users)
  , p_name
  , p_age)
  returning user_id;
$add_user$
language sql
security definer;


create function del_user(p_user_id bigint) returns void as
$del_user$
  update users
  set is_active = false
  where user_id = p_user_id;
$del_user$
language sql
security definer;

