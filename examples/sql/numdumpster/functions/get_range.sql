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
