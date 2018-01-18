CREATE TABLE "institution" (
  "party_id" integer NOT NULL Primary Key References "party",
  "longitude" float NOT NULL, -- TODO: Check range; decimal?
  "latitude" float NOT NULL  -- TODO: check range; decimal?
);
COMMENT ON TABLE "institution" IS 'Institution specific party data';
