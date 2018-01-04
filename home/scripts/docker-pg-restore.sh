#!/bin/bash

# todo: this.
# for whatever reason this creates a db named plm_sanitizer_work and we need to rename it.  maybe it's a bad snapshot?
db="$(ls ~/db_dumps/plm_development*.pgdump* --sort=time | head -n 1)"
echo Loading from $db
docker exec -i docker_postgres_1 pg_restore -C -c --no-acl --no-owner -Upostgres -d plm_development --if-exists  < $db

