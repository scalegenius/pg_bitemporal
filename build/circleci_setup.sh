#!/bin/bash
#
# Create Database
#
source  `dirname $0`/environment.sh

CREATE=$(cat <<EOF
-- Properly quote variables.
-- Names use double quotes
-- String values use single quote
\set PGUSER :"PGUSER"
\set PGDATABASE :"PGDATABASE"
\set PGPASSWORD :'PGPASSWORD'

create user :PGUSER password :PGPASSWORD superuser;
create database :PGDATABASE owner :PGUSER;
EOF
)


# Use Postgres SuperUser for this part
PSQL="$PSQL -U postgres -d template1 -h $PGHOST "

if [ "x$CIRCLECI" == "x" ]; then
   echo "Skipping PostgreSQL setup"
else
  echo "Setup Database"

  #
  # create .pgpass
  #
  PGPASS=${HOME}/.pgpass
  # echo "hostname:port:database:username:password"
  echo "*:*:*:$PGUSER:$PGPASSWORD" >> ${PGPASS}
  echo "*:*:template1:postgres:$PGPASSWORD" >> ${PGPASS}
  chmod 600  ${PGPASS}
  if [ ! -f ${PGPASS} ]; then
      exit 3
  fi

  PGUSER=${PGUSER:-$USER}
  PGPASSWORD=${PGPASSWORD:-password}
  PGDATABASE=${PGDATABASE:-$PGUSER}

  echo "\\set PGUSER $PGUSER %\\set PGDATABASE $PGDATABASE%\\set PGPASSWORD $PGPASSWORD% $CREATE " |tr '%' '\n' | $PSQL
  exit $?
fi


