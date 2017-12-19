alter table "volume_access" add column "share_full" boolean null;
alter table audit."volume_access" add column "share_full" boolean null;


-- migrate all existing entries such that share_full = true when party is everybody



-- check constraint that value is not null when party is everbody?

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
