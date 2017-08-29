create table t (f_id serial, f_name varchar(64), f_script varchar);
insert into t (f_name, f_script) values ('foo', 'create function foo() bigint as $$ select 42; $$ language sql;');
insert into t (f_name, f_script) values ('bar', 'create function bar() bigint as $$ select 21; $$ language sql;');
commit;

perform 'create function baz() returns void as $$ select 7; $$';
perform '; create function qux() returns void as $$ select 1; $$; ';

