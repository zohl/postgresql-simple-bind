create table accounts (
  account_id bigint   primary key
, name       varchar  not null
);

insert into accounts(account_id, name)
  select nextval('messages_message_id_seq') as account_id
       , t.name
  from (
    select distinct sender as name from messages
    union
    select distinct receiver as name from messages) t;


alter table messages
  add column sender_id bigint
, add column receiver_id bigint;

update messages set
  sender_id = (select account_id from accounts a where name = sender)
, receiver_id = (select account_id from accounts a where name = receiver);

alter table messages
  add constraint msg_sender_id_fk foreign key (sender_id) references accounts(account_id)
, add constraint msg_receiver_id_fk foreign key (receiver_id) references accounts(account_id)
, drop column sender
, drop column receiver;


create or replace function send_message(p_receiver varchar, p_contents varchar default null) returns bigint as
$send_message$
  insert into messages(sender_id, receiver_id, contents)
  values (
    (select account_id from accounts where name = user)
  , (select account_id from accounts where name = p_receiver)
  , p_contents)
  returning message_id;
$send_message$
language sql
security definer;


create or replace function get_new_messages(p_receiver varchar) returns table (
  message_id  bigint
, sender      varchar
, contents    varchar
) as
$get_new_messages$
  select message_id, acc_sender.name, contents
  from messages msg
     , accounts acc_sender
     , accounts acc_receiver
  where msg.sender_id = acc_sender.account_id
    and msg.receiver_id = acc_receiver.account_id
    and acc_receiver.name = p_receiver
    and msg.is_read = false
  order by creation_time;
$get_new_messages$
language sql
security definer;


create or replace function mark_as_read(p_receiver varchar, p_message_id bigint) returns void as
$mark_as_read$
  update messages set is_read = true
  where receiver_id in (select account_id from accounts where name = p_receiver)
    and message_id = p_message_id;
$mark_as_read$
language sql
security definer;

