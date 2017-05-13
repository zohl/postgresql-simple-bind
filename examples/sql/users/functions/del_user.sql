create function del_user(p_user_id bigint) returns void as
$del_user$
  update users
  set is_active = false
  where user_id = p_user_id;
$del_user$
language sql
security definer;
