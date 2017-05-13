create function get_new_messages(p_receiver varchar) returns table (
  message_id  bigint
, sender      varchar
, contents    varchar
) as
$get_new_messages$
  select message_id, sender, contents
  from messages
  where receiver = p_receiver
    and is_read = false;
$get_new_messages$
language sql
security definer;
