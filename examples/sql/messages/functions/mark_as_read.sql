create function mark_as_read(p_receiver varchar, p_message_id bigint) returns void as
$mark_as_read$
  update messages set is_read = true
  where receiver = p_receiver
    and message_id = p_message_id;
$mark_as_read$
language sql
security definer;

