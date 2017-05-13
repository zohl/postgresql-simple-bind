alter table messages add column creation_time timestamp default current_timestamp;

create or replace function get_new_messages(p_receiver varchar) returns table (
  message_id  bigint
, sender      varchar
, contents    varchar
) as
$get_new_messages$
  select message_id, sender, contents
  from messages
  where receiver = p_receiver
    and is_read = false
  order by creation_time asc;
$get_new_messages$
language sql
security definer;

