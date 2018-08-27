CREATE OR REPLACE VIEW "volume_access_view" ("volume", "party", "access", "share_full") AS
SELECT volume
     , party
     , individual
     , share_full
FROM volume_access
UNION ALL
SELECT mx.volume
     , mx.child
     , vap.result_perm
     , vap.share_full
FROM 
  (
	 SELECT volume
	       , max(parent) as mparent 
	       , child
	  FROM (
		  SELECT acc.volume
		       , aut.parent
		       , aut.child
		       , LEAST(acc.children,
			       CASE WHEN acc.children <= 'SHARED'
			       THEN aut.site
			       ELSE aut.member END)
			   as result_perm
		       , acc.share_full
		  FROM volume_access acc
		    JOIN authorize_view aut ON party = parent
	   ) AS vap
	  WHERE NOT EXISTS
	    (SELECT *
	     FROM 
	       (
		  SELECT acc.volume
		       , aut.parent
		       , aut.child
		       , LEAST(acc.children,
			       CASE WHEN acc.children <= 'SHARED'
			       THEN aut.site
			       ELSE aut.member END)
			   as result_perm
		       , acc.share_full
		  FROM volume_access acc
		    JOIN authorize_view aut ON party = parent
	       ) AS v2
	     WHERE (vap.volume, vap.child) = (v2.volume, v2.child)
	     AND v2.result_perm > vap.result_perm)
	  GROUP BY volume, child
   ) AS mx
  JOIN 
   (
	SELECT acc.volume
		       , aut.parent
		       , aut.child
		       , LEAST(acc.children,
			       CASE WHEN acc.children <= 'SHARED'
			       THEN aut.site
			       ELSE aut.member END)
			   as result_perm
		       , acc.share_full
		  FROM volume_access acc
		    JOIN authorize_view aut ON party = parent
   )
   vap on (mx.volume, mx.mparent, mx.child) = (vap.volume, vap.parent, vap.child);
