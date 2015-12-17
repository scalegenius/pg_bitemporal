-- Start a transaction.
BEGIN;
SELECT plan( 2 );

-- two simple tests to check pgtap is working
SELECT ok(
    now() = now(),
    'now() = now() should return true'
);

SELECT is(
    ARRAY(
        VALUES (1),(2),(3)
    ),
    ARRAY[ 1,2, 3 ],
    'An array of 3 values'
);

SELECT * FROM finish();
ROLLBACK;


