-- remove check constraint?

alter table audit."volume_access" drop column "share_full";
alter table "volume_access" drop column "share_full";
