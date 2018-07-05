delete from notify where notice in (select id from notice where name in ('CommentVolume', 'CommentReply'));
delete from notification where notice in (select id from notice where name in ('CommentVolume', 'CommentReply'));
delete from notice where name in ('CommentVolume', 'CommentReply');
alter table notification drop column comment;

drop trigger comment_changed on comment;
drop function comment_refresh();
drop materialized view comment_thread;
drop table comment;
