#!/bin/bash

export BUILD=`dirname $0`
export APP_ROOT=`dirname $BUILD`
export PGOPTIONS="--client-min-messages=warning"

export PSQL_CMD="psql -q -P pager=off "
export PSQL="$PSQL_CMD -v VERBOSITY=terse -v ON_ERROR_STOP=on"

export PGHOST=${PGHOST:-localhost}
export PGPORT=${PGPORT:-5432}
export PGUSER=${PGUSER:-ubuntu}
export PGDATABASE=circle_test

export PGPASSWORD=$POSTGRES_PASSWORD
export PGVER=${POSTGRES_VERSION:-9.6}

export DATABASE_URL=postgres://${PGUSER}:${PGPASSWORD}@${PGHOST}:${PGPORT}/${PGDATABASE}
