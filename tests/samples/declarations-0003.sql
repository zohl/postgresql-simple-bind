
-- foo
--   arguments: none
--   return value: constant integer

create function foo()
returns bigint
as $$
  select 42::bigint
$$ language 'sql';


/* bar
     arguments: none
     return value: constant integer, half of `foo()`
*/

create function bar()
returns bigint
as $$
  select 21::bigint
$$ language 'sql';

