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
