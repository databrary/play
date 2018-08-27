alter table "volume_access" add column "share_full" boolean null;
alter table audit."volume_access" add column "share_full" boolean null;

-- migrate all existing entries such that share_full = true when party is everybody
update volume_access
set share_full = true
where (party, individual, children) = (-1, 'PUBLIC', 'PUBLIC');

ALTER TABLE "volume_access" ADD CONSTRAINT "volume_access_check_full1" CHECK ( party = -1 OR share_full is null );
ALTER TABLE "volume_access" ADD CONSTRAINT "volume_access_check_full2" CHECK ( party != -1 OR individual != 'PUBLIC' OR share_full is not null );

CREATE OR REPLACE VIEW "volume_access_view" ("volume", "party", "access", "share_full") AS
	SELECT volume, party, individual, share_full FROM volume_access

        UNION ALL

        SELECT volume
             , child
             , MAX(LEAST(children, CASE WHEN children <= 'SHARED' THEN site ELSE member END))
             , BOOL_AND(share_full) -- only parent everybody has a value
	  FROM volume_access
            JOIN authorize_view ON party = parent
        GROUP BY volume, child;
