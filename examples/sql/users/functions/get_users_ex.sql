create function get_users_ex(
  in  p_filter varchar2 default ''
, out p_user_id bigint
, out p_name varchar2
, out p_age bigint
) returns setof record as
$get_users_ex$
  select user_id, name, age
  from users
  where is_active = true
    and name like nvl(p_filter, name)
  order by user_id;
$get_users_ex$
language sql
security definer;
