create function send_message(p_receiver varchar, p_contents varchar default null) returns bigint as
$send_message$
  insert into messages(sender, receiver, contents)
  values (
    current_user
  , p_receiver
  , p_contents)
  returning message_id;
$send_message$
language sql
security definer;
