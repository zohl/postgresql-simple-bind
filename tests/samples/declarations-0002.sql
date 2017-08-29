create function foo()
returns bigint
as $$
  select 42::bigint
$$ language 'sql';

create function bar()
returns bigint
as $$
  select 21::bigint
$$ language 'sql';

