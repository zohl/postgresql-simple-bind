create function get_last_num() returns bigint as
$get_last_num$
  select x
  from numdumpster nd
  order by nd.id desc
  limit 1;
$get_last_num$
language sql
security definer;
