create function get_val(p_number bigint default '14') returns bigint as
$$
  select p_number;
$$
language sql;


