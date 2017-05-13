create function nvl(p varchar2, alt varchar2) returns varchar2 as
$nvl$
  select (case p when '' then alt else p end);
$nvl$
language sql;
