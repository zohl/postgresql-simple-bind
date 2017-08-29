create function foo()
returns bigint as $$
  select 42::bigint
$$ language 'sql';

