create table messages (
  message_id   bigserial primary key
, sender       varchar   not null
, receiver     varchar   not null
, contents     varchar
, is_read      boolean   default false
);
