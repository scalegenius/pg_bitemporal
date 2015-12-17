BEGIN;
set client_min_messages to warning;
set local search_path = 'temporal_relationships','public';

SELECT plan( 27 );

-- support functions: xor, fst, snd
select is( xor(true, true), false) ; 
select is( xor(true, false), true) ; 
select is( xor(false, true), true) ; 
select is( xor(false, false), false) ; 

select is( fst(  daterange('1999-01-01'::date, '2010-12-31'::date)), '1999-01-01'::date );
select is( snd(  daterange('1999-01-01'::date, '2010-12-31'::date)), '2010-12-31'::date );


--------------------------
-- setup sample data
select lives_ok($$
create view raw_testing_data (description,s1,f1,s2,f2, force_order) as
values 
('-- starts'
,'1999-01-01'::date
,'1999-01-10'::date
,'1999-01-01'::date
,'1999-01-22'::date
,1),(
'-- starts^-1'
,'1999-02-01'::date
,'1999-02-10'::date
,'1999-02-01'::date
,'1999-02-22'::date
,2),(
'--finish'
,'1999-01-22'::date
,'1999-02-28'::date
,'1999-01-10'::date
,'1999-02-28'::date
,3),(

'--finish^-1'
,'1999-04-22'::date
,'1999-05-28'::date
,'1999-04-10'::date
,'1999-05-28'::date
,4),(

'-- equals'
,'2000-01-01'::date
,'2000-01-02'::date
,'2000-01-01'::date
,'2000-01-02'::date
,5),(

'--during'
,'2020-01-02'::date
,'2020-01-03'::date
,'2020-01-01'::date
,'2020-01-31'::date
,6),(

'-- during^-1'
,'2020-03-01'::date
,'2020-03-31'::date
,'2020-03-02'::date
,'2020-03-12'::date
,7),(

'-- overlaps'
,'1999-01-01'::date
,'2010-12-31'::date
,'2000-01-01'::date
,'2020-12-31'::date
,8),(

'--overlaps^-1'
,'2000-01-01'::date
,'2002-12-31'::date
,'1999-01-01'::date
,'2001-12-31'::date
,9),(

'--before'
,'1999-01-01'::date
,'1999-01-10'::date
,'2001-12-01'::date
,'2001-12-31'::date
,10),(

'--before^-1'
,'2012-12-01'::date
,'2012-12-31'::date
,'1999-05-11'::date
,'1999-07-11'::date
,11),(

'--meets'
,'2002-10-01'::date
,'2002-11-01'::date
,'2002-11-01'::date
,'2002-11-30'::date
,12),(

'--meets^-1'
,'2020-01-01'::date
,'2020-01-31'::date
,'2019-12-01'::date
,'2020-01-01'::date
,13) 
;
create view testing_relationships (description, s1,f1,s2,f2, a, b) as
select description,s1,f1,s2,f2, daterange(s1,f1, '[)'), daterange(s2,f2,'[)') 
, force_order
from raw_testing_data order by force_order
;
$$);

select bag_eq( $q$ select count(*) from testing_relationships $q$::text,
$$ values ( 13) $$ );

-- 
-- Test each relationship function against the sample data
--

select results_eq(
$q$
      select 
        has_starts(a , b) 
      from testing_relationships
 $q$::text,
$$ values  
  (true), (true)
, (false), (false), (false)
, (false), (false), (false), (false)
, (false), (false), (false), (false)
$$,'has_starts'
);

select results_eq(
$q$
     select
      has_finishes(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (true), (true)
, (false)
, (false), (false), (false), (false)
, (false), (false), (false), (false)
$$, 'has_finishes'
);
select results_eq(
$q$
     select
      equals(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false)
, (true)
, (false), (false), (false), (false)
, (false), (false), (false), (false)
$$, 'equals'
);
/* test 12 */
select results_eq(
$q$
     select
      is_during(a , b )
     from testing_relationships 
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (true), (false)
, (false), (false)
, (false), (false), (false), (false)
$$, 'is_during'
);
select results_eq(
$q$
     select
      is_contained_in(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (true), (false), (false)
, (false), (false), (false), (false)
$$, 'is_contained_in'
);
select results_eq(
$q$
     select
      has_during(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (true), (true), (false), (false)
, (false), (false), (false), (false)
$$, 'has_during'
);

select results_eq(
$q$
     select
      is_overlaps(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (true), (false)
, (false), (false), (false), (false)
$$, 'is_overlaps'
);
select results_eq(
$q$
     select
      is_overlaps(b,a )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (false), (true)
, (false), (false), (false), (false)
$$, 'is_overlaps'
);
select results_eq(
$q$
     select
      has_overlaps(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (true), (true)
, (false), (false), (false), (false)
$$, 'has_overlaps'
);
select results_eq(
$q$
     select
      is_before(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (false), (false)
, (true), (false), (false), (false)
$$, 'is_before'
);
select results_eq(
$q$
     select
      is_after(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (false), (false)
, (false), (true), (false), (false)
$$, 'is_after'
);
select results_eq(
$q$
     select
      has_before(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (false), (false)
, (true), (true), (false), (false)
$$, 'has_before'
);
select results_eq(
$q$
     select
      is_meets(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (false), (false)
, (false), (false), (true), (false)
$$, 'is_meets'
);
select results_eq(
$q$
     select
      has_meets(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (false), (false)
, (false), (false), (true), (true)
$$, 'has_meets'
);

-- partition functions

select results_eq(
$q$
     select
      has_includes(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (true), (true)
, (true), (true), (true)
, (true), (true), (true), (true)
, (false), (false), (false), (false)
$$, 'has_includes'
);
select results_eq(
$q$
     select
      has_contains(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (true), (true)
, (true), (true), (true)
, (true), (true), (false), (false)
, (false), (false), (false), (false)
$$, 'has_contains'
);
select results_eq(
$q$
     select
      has_aligns_with(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (true), (true)
, (true), (true), (false)
, (false), (false), (false), (false)
, (false), (false), (false), (false)
$$, 'has_aligns_with'
);
select results_eq(
$q$
     select
      has_encloses(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (true), (true)
, (true), (true), (false)
, (true), (true), (false), (false)
, (false), (false), (false), (false)
$$, 'has_encloses'
);
select results_eq(
$q$
     select
      has_excludes(a , b )
     from testing_relationships
$q$::text,
$$ values  
  (false), (false)
, (false), (false), (false)
, (false), (false), (false), (false)
, (true), (true), (true), (true)
$$, 'has_excludes'
);

SELECT * FROM finish();
ROLLBACK;
-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
