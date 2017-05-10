create function clear() returns void as
$clear$
  delete from numdumpster;
$clear$
language sql
security definer;
