#!/bin/bash

# todo: this.
# for whatever reason this creates a db named plm_sanitizer_work and we need to rename it.  maybe it's a bad snapshot?
db="$(ls ~/db_dumps/plm_development*.pgdump* --sort=time | head -n 1)"
echo Loading from $db

docker exec -i docker_postgres_1 dropdb -Upostgres plm_sanitizer_work --if-exists
docker exec -i docker_postgres_1 dropdb -Upostgres plm_development --if-exists
docker exec -i docker_postgres_1 createdb -Upostgres plm_sanitizer_work
docker exec -i docker_postgres_1 createdb -Upostgres plm_development
docker exec -i docker_postgres_1 pg_restore -e -C -c --no-acl --no-owner -Upostgres -d plm_development < "$db"
docker exec -i docker_postgres_1 dropdb -Upostgres plm_development --if-exists
docker exec -i docker_postgres_1 psql -Upostgres -c 'alter database plm_sanitizer_work rename to plm_development'

# bin/rails plm:users:create_all
# bin/rails db:create RAILS_ENV=test
# bin/rails db:environment:set RAILS_ENV=development

# -j8 doesn't work with standard input
# try volumes in docker/docker-compose.yml so we can -j8 it.
# also try persisting pg data in volume
# rake stuff?

