BEGIN;
SELECT plan( 2 );


CREATE TABLE postgres_auth_methods (
    auth_method text NOT NULL unique
);

\d postgres_auth_methods
COPY postgres_auth_methods (auth_method) FROM stdin;
md5
ldap
krb
ident
ident pgadmin
ident sameuser
\.

select lives_ok($$

select bitemporal_internal.pk_constraint('release_version_id') ;
select bitemporal_internal.unique_constraint('release_version') ;

CREATE TABLE database_versions (
 release_version_id       integer  --  | not null default nextval('database_versions_id_seq'::regclass)
 , release_version   numeric(3,1)  -- temporal unique
 , effective                    temporal_relationships.timeperiod
 , asserted                     temporal_relationships.timeperiod
 , row_created_at               timestamptz    NOT NULL DEFAULT now()
 , CONSTRAINT "bitemporal ok release_version_id unique idx" EXCLUDE
      USING gist (release_version_id WITH =, asserted WITH &&, effective WITH &&)
 , CONSTRAINT "bitemporal pk release_version_id"
        check(true or 'pk' <> '@release_version_id@')
, CONSTRAINT "bitemporal unique release_version" EXCLUDE USING gist
                 (release_version WITH =, asserted WITH &&, effective WITH &&)
);

select bitemporal_internal.pk_constraint('postgres_cluster_id');
select bitemporal_internal.unique_constraint('port');
select bitemporal_internal.fk_constraint(
            'postgres_version', 'database_versions', 'release_version');

CREATE TABLE postgres_clusters (
    postgres_cluster_id          integer NOT NULL -- bitemporal PK
  , port                         int              -- bitemporal unique
  , name                         varchar(16)
  , postgres_version             int              -- bitemporal fk
  , archive                      boolean   DEFAULT false NOT NULL
  , preferred_auth_method        text      references  postgres_auth_methods ( auth_method )
  , effective                    temporal_relationships.timeperiod
  , asserted                     temporal_relationships.timeperiod
  , row_created_at               temporal_relationships.time_endpoint NOT NULL DEFAULT now()
  , CONSTRAINT "bitemporal pk postgres_cluster_id unique idx" EXCLUDE
       USING gist (postgres_cluster_id WITH =, asserted WITH &&, effective WITH &&)
  , CONSTRAINT "bitemporal pk postgres_cluster_id" check(true or 'pk' <> '@postgres_cluster_id@')
  , CONSTRAINT "bitemporal unique port" EXCLUDE USING gist
      (port WITH =, asserted WITH &&, effective WITH &&)
  , CONSTRAINT "bitemporal fk postgres_versiondatabase_versionsrelease_version"
      check (true or 'fk' <> '@postgres_version -> database_versions(release_version)@')
);
$$);

select results_eq($q$ 
/* relname,conname,contype,consrc */
select relname::name, conname::name, contype::char,pg_get_constraintdef(pg_constraint.oid) 
from pg_constraint join pg_class on pg_class.oid = conrelid
  where 
    conname like 'bitemporal%'
  and relname in ( 'postgres_clusters' , 'database_versions' )
  order by 1,2
$q$,$v$
VALUES 
 ('database_versions'::name,'bitemporal ok release_version_id unique idx'::name
    ,'x'::char,'EXCLUDE USING gist (release_version_id WITH =, asserted WITH &&, effective WITH &&)')
,('database_versions'::name,'bitemporal pk release_version_id'::name
    ,'c'::char
    ,$src$CHECK ((true OR ('pk'::text <> '@release_version_id@'::text)))$src$)
,('database_versions'::name,'bitemporal unique release_version'::name
  ,'x'::char,'EXCLUDE USING gist (release_version WITH =, asserted WITH &&, effective WITH &&)')
,('postgres_clusters'::name
  ,'bitemporal fk postgres_versiondatabase_versionsrelease_version'::name
  ,'c'::char
  ,$$CHECK ((true OR ('fk'::text <> '@postgres_version -> database_versions(release_version)@'::text)))$$)
,('postgres_clusters'::name
  ,'bitemporal pk postgres_cluster_id'::name,'c'::char
  ,$$CHECK ((true OR ('pk'::text <> '@postgres_cluster_id@'::text)))$$)
,('postgres_clusters'::name
 ,'bitemporal pk postgres_cluster_id unique idx'::name
  ,'x'::char,NULL::text)
,('postgres_clusters'::name,'bitemporal unique port'::name
   ,'x'::char,NULL::text)
$v$);

/*
      relname      |                            conname                             | contype |                                           consrc                                           
-------------------+----------------------------------------------------------------+---------+--------------------------------------------------------------------------------------------
 database_versions | bitemporal ok release_version_id unique idx                    | x       | 
 database_versions | bitemporal pk release_version_id                               | c       | (true OR ('pk'::text <> '@release_version_id@'::text))
 database_versions | bitemporal unique release_version                              | x       | 
 postgres_clusters | bitemporal fk postgres_versiondatabase_versionsrelease_version | c       | (true OR ('fk'::text <> '@postgres_version -> database_versions(release_version)@'::text))
 postgres_clusters | bitemporal pk postgres_cluster_id                              | c       | (true OR ('pk'::text <> '@postgres_cluster_id@'::text))
 postgres_clusters | bitemporal pk postgres_cluster_id unique idx                   | x       | 
 postgres_clusters | bitemporal unique port                                         | x       | 
(7 rows)
*/




SELECT * FROM finish();
ROLLBACK;

-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
