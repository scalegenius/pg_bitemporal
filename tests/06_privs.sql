-- Start a transaction.
BEGIN;
SELECT plan( 2 );

SELECT schema_privs_are ( 
 'temporal_relationships'
, 'public'
, ARRAY['USAGE']
-- , ':description 
);


SELECT schema_privs_are ( 
'bitemporal_internal'
, 'public'
, ARRAY['USAGE']
-- , ':description 
);

SELECT * FROM finish();
ROLLBACK;


