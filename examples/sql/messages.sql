create sequence s_id;

create table messages (
  message_id   bigint   primary key
, sender       varchar  not null
, receiver     varchar  not null
, contents     varchar
, is_read      boolean  default false
);

create function send_message(p_receiver varchar, p_contents varchar default null) returns bigint as
$send_message$
  insert into messages(message_id, sender, receiver, contents)
  values (
    nextval('s_id')
  , current_user
  , p_receiver
  , p_contents)
  returning message_id;
$send_message$
language sql
security definer;

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

create function mark_as_read(p_receiver varchar, p_message_id bigint) returns void as
$mark_as_read$
  update messages set is_read = true
  where receiver = p_receiver
    and message_id = p_message_id;
$mark_as_read$
language sql
security definer;

