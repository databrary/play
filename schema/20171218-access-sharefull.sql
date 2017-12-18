alter table "volume_access" add column "share_full" boolean null;
alter table audit."volume_access" add column "share_full" boolean null;


-- migrate all existing entries such that share_full = true when party is everybody

-- check constraint that value is not null when party is everbody?
