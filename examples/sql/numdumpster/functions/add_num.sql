create function add_num(p_x bigint) returns void as
$add_num$
  insert into numdumpster(id, x)
    select coalesce(max(nd.id), 0) + 1, p_x from numdumpster nd;
$add_num$
language sql
security definer;
