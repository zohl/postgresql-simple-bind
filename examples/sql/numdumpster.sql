create table numdumpster (
  id  bigint primary key
, x   bigint
);

create function get_last_num() returns bigint as
$get_last_num$
  select x
  from numdumpster nd
  order by nd.id desc
  limit 1;
$get_last_num$
language sql
security definer;


create function get_all_nums() returns setof bigint as
$get_all_nums$
  select x
  from numdumpster nd
  order by nd.id asc;
$get_all_nums$
language sql
security definer;


create function get_range(
    p_range_min bigint default null
  , p_range_max bigint default null
) returns setof bigint as
$get_all_nums$
  select x
  from numdumpster nd
  where x >= coalesce(p_range_min, x)
    and x <= coalesce(p_range_max, x)
  order by nd.id asc;
$get_all_nums$
language sql
security definer;


create function add_num(p_x bigint) returns void as
$add_num$
  insert into numdumpster(id, x)
    select coalesce(max(nd.id), 0) + 1, p_x from numdumpster nd;
$add_num$
language sql
security definer;


create function clear() returns void as
$clear$
  delete from numdumpster;
$clear$
language sql
security definer;

create function get_val(p_number bigint default '14') returns bigint as
$$
  select p_number;
$$
language sql;


