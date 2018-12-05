#!/bin/bash
#
source  `dirname $0`/environment.sh

D=${APP_ROOT}/sql

# infra setup
if [ "${CI:-nil}" != "nil" ] ; then
   echo "CI Infrastructure setup"
fi
echo "_load_all.sql"
echo "\\set CI $CI %\\ir $D/_load_all.sql" |tr '%' '\n' | $PSQL
LOAD_ALL=$?

if (( $LOAD_ALL )); then
  exit 1
fi

# load pgtap framework
LOAD_PGTAP=0
if  [ "${CI:-nil}" != "nil" ] ; then
  echo "Load pgtap"
  echo "\\ir ${BUILD}/pgtap.sql %\\ir ${BUILD}/pgtap_ft.sql" |tr '%' '\n' | $PSQL
  LOAD_PGTAP=$?
fi

if ((  $LOAD_PGTAP || $LOAD_ALL )); then
  exit 1
else
  exit 0
fi
