create function get_all_nums() returns setof bigint as
$get_all_nums$
  select x
  from numdumpster nd
  order by nd.id asc;
$get_all_nums$
language sql
security definer;
