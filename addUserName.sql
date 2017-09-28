alter table account add "username" varchar(256);
alter table audit.account add "username" varchar(256) not null default '';
-- update audit.account set username = '';
update account set username = email;  -- should use app level update
alter table audit.account alter "username" drop default;
alter table account alter "username" set not null;
alter table account add unique("username");
