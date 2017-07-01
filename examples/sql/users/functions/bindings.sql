create function add_user(p_name varchar2, p_age bigint) returns bigint as
$add_user$
  insert into users(user_id, name, age)
  values (
    (select coalesce(max(user_id), 0) + 1 from users)
  , p_name
  , p_age)
  returning user_id;
$add_user$
language sql
security definer;

create function del_user(p_user_id bigint) returns void as
$del_user$
  update users
  set is_active = false
  where user_id = p_user_id;
$del_user$
language sql
security definer;

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

create function get_users(p_filter varchar2 default '') returns setof t_user as
$get_users$
  select user_id, name, age
  from users
  where is_active = true
    and name like nvl(p_filter, name)
  order by user_id;
$get_users$
language sql
security definer;
