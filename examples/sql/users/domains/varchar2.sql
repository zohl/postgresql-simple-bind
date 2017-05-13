create domain varchar2 as varchar
  default ''
  not null
  check (length(value) <= 4000);
