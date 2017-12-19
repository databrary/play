
DROP VIEW "volume_access_view";

CREATE VIEW "volume_access_view" ("volume", "party", "access") AS
	SELECT volume, party, individual FROM volume_access
	UNION ALL
	SELECT volume, child, MAX(LEAST(children, CASE WHEN children <= 'SHARED' THEN site ELSE member END))
	  FROM volume_access JOIN authorize_view ON party = parent GROUP BY volume, child;

-- remove check constraint?

alter table audit."volume_access" drop column "share_full";
alter table "volume_access" drop column "share_full";

