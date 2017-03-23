#!/bin/bash

# returns the name of the database to use for the current branch
# if it's a brand new one, create it!

# should this be in ruby for ease of this team's development.

BASE_BRANCH="current"     # this branch uses the base db.  run migrations against it to keep your base db up to date.
BASE_DB="plm_development" # all DBs will be cloned from this one
DB_PREFIX="plm_dev"       # dbs created by this script will be in this namespace
SPARE_DB="spare"          # rename me instead of copying, then spin up new spare.

usage() {
  cat <<EOF

feature_db.sh # Manages databases per feature branch

Usage:
feature_db.sh # print current branch, creating it if needed.  Put this in your database.yml.erb
-l --ls --list # Print list of databases
-d --delete # Delete managed databases
-h --help # Prints this help text

See also
Global vars at the top of this file control db and branch prefs.


EOF
}

cli() {
  case "$1" in
    '')               main               ;;
    -l|--ls|--list)   list_managed_dbs   ;;
    -d|--delete)      drop_managed_dbs   ;;
    -h|--help|*)      usage              ;;
  esac
}

main() {
  USE_DB="$(db_name)"

  if ! db_exists $USE_DB ; then
    prepare_db $USE_DB
  fi

  echo $USE_DB
}

git_branch_name() {
  git rev-parse --abbrev-ref HEAD
}

db_exists() {
  db_name=$1
  psql -lqt |
    awk '{print $1}' |
    grep "^${db_name}$" &>/dev/null
}

db_name() {
  if [[ "$(git_branch_name)" == "$BASE_BRANCH" ]] ; then
    echo "$BASE_DB"
  else
    echo "$(feature_db_name)"
  fi
}

feature_db_name() {
  echo "${DB_PREFIX}_$(git_branch_name)"
}

prepare_db() {
  target_db=$1

  # use a spare if it's available
  if db_exists "$SPARE_DB" ; then
    rename_db "$SPARE_DB" "$target_db"
  else
    createdb -T $BASE_DB $target_db
  fi

  make_a_spare & # always prepare a new spare for next time
}

rename_db() {
  src=$1
  dst=$2

  echo "ALTER DATABASE $src RENAME TO $dst" | psql $BASE_DB
}

list_managed_dbs() {
  psql -lqt |
    awk '{print $1}' |
    grep "^${DB_PREFIX}_" |
    grep -v "^$BASE_DB"
}

drop_managed_dbs() {
  echo -e "\n\033[5mDropping databases: (ctrl-c to cancel)\033[0m"
  list_managed_dbs
  echo
  for n in 5 4 3 2 1 ; do echo $n ; sleep 1 ; done

  list_managed_dbs | xargs -n1 dropdb --echo
}

drop_old_dbs() {
  # drop all the dbs starting with prefix
  # if their migration column is older that current's
  # select max(version::bigint) from schema_migrations;

  :
}

make_a_spare() {
  # always do this in the background
  createdb -T $BASE_DB "_$SPARE_DB" # how to prevent if already running?
  rename_db "_$SPARE_DB" "${DB_PREFIX}_${SPARE_DB}"
}


cli $@

#### TODO
# what to do about test db
# robustness.  mainly pg perms.
