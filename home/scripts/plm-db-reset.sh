#!/bin/sh

OPTS="--host postgres.service.docker -Upostgres"
dropdb $OPTS plm_development --if-exists &&
  createdb $OPTS plm_development &&
  # bin/rake db:schema:load &&
  pg_restore $OPTS -d plm_development ~/plm_development_clean_snapshots.pgdump.20170818 -j7 --no-owner  --no-privileges -Upostgres --disable-triggers  --superuser=postgres
