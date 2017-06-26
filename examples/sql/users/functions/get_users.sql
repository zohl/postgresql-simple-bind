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
