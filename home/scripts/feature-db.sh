#!/bin/bash

# returns the name of the database to use for the current branch
# if it's a brand new one, create it!

# should this be in ruby for ease of this team's development.

# TODO:
# fail gracefully if createdb bombs
#   [1574_omics_basic_study_page]: createdb: database creation failed: ERROR:  source database “plm_development” is being accessed by other users
#   DETAIL:  There are 4 other sessions using the database.
#   ERROR:  database “plm_dev_spare_build_in_progress” does not exist


#   can't get around the createdb if in use thing.  killing rails kinda helps.  having a spare ready helps (although master in use could block it)
#     having a spare that isn't being touched bypasses the problem.  and then once we've switched branches, master is free to copy from.
#     we *could* do a copy instead of a template.  how much slower is that?

# Make -h default.  Rails should ask for db name explicitly.

# Done
# Check if we're in a good dir
# -s verbiage is a bit off.  Not obvious 1st, 2nd lines connect.

BASE_BRANCH="master"      # this branch uses the base db.  run migrations against it to keep your base db up to date.
BASE_DB="plm_development" # all DBs will be cloned from this one
DB_PREFIX="plm_dev"       # dbs created by this script will be in this namespace
SPARE_DB="spare"          # rename me instead of copying, then spin up new spare.

export VERBOSE="$VERBOSE" # print logs

usage() {
  cat <<EOF

feature_db.sh # Manages databases per feature branch

Usage:
feature_db.sh     # print current branch, creating it if needed.  Put this in your database.yml.erb
-l --ls --list    # Print list of databases
-d --delete       # Delete managed databases, keep the spare
-D --force-delete # Delete all managed dbs
-h --help         # Prints this help text
-s --status       # Shows what featuredb will do when run on this branch
-c --create-spare # Creates a spare now

VERBOSE=1 feature_db.sh # Print logs to stderr.  Yes this can go in your db.yml.

See also:
Global vars at the top of this file control db and branch prefs.

Rails:
To use this for rails development, add this line to your config/database.yml
  database: <%=  %x{feature-db.sh}.chomp %>
EOF
}


cli() {
  case "$1" in
    '')                 main                 ;;
    -l|--ls|--list)     list_managed_dbs     ;;
    -d|--delete)        drop_managed_dbs     ;;
    -D|--force-delete)  drop_all_managed_dbs ;;
    -s|--status)        status               ;;
    -c|--create-spare)  make_a_spare         ;;
    -h|--help|*)        usage                ;;
  esac
}

main() {
  USE_DB="$(db_name)"

  if ! db_exists $USE_DB ; then
    prepare_db $USE_DB
  fi

  log "using db: $USE_DB"
  echo $USE_DB
}

rails_check() {
  [[ -f './config/database.yml' ]] || (
    echo 'Could not find database.yml.  Please run this in the top level of your rails app.'
    exit 1
  )
}

status() {
  USE_DB="$(db_name)"

  echo "This branch will use '$USE_DB'"
  if db_exists $USE_DB ; then
    echo "'$USE_DB' already exists"
  else
    echo "'$USE_DB' needs to be created"
  fi

  if db_exists $(spare_db_name) ; then
    echo "A spare db is ready ($(spare_db_name))"
  elif db_exists $(temp_spare_db) ; then
    echo "A spare db is being set up ($(temp_spare_db))"
  else
    echo 'The spare db needs to be created'
  fi
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
  if db_exists "$(spare_db_name)" ; then
    log 'copying spare db'
    rename_db "$(spare_db_name)" "$target_db"
  else
    log 'cloning master db'
    createdb -T $BASE_DB $target_db
  fi

  log 'setting up a spare'
  feature-db.sh --create-spare &>/dev/null &
  log 'created spare db'
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
  for n in 3 2 1 ; do echo $n ; sleep 1 ; done

  list_managed_dbs | grep -v $(spare_db_name) | xargs -n1 dropdb --echo
}

drop_all_managed_dbs() {
  echo "Dropping ALL managed dbs"
  drop_managed_dbs
  dropdb --if-exists $(spare_db_name)
  dropdb --if-exists $(temp_spare_db)
}

# drop_old_dbs() {
  # todo: is this worthwhile?
  # drop all the dbs starting with prefix

  # if their migration column is older that current's
  # select max(version::bigint) from schema_migrations;

#   :
# }

make_a_spare() {
  log 'building spare'
  db_exists $(spare_db_name) && return
  db_exists $(temp_spare_db) && return

  # always do this in the background...
  createdb -T $BASE_DB "$(temp_spare_db)"
  rename_db "$(temp_spare_db)" "$(spare_db_name)"
  log 'finished spare'
}

spare_db_name() {
  echo "${DB_PREFIX}_${SPARE_DB}"
}

# don't give the spare its real name until its ready to use
temp_spare_db() {
  echo "${DB_PREFIX}_${SPARE_DB}_build_in_progress"
}

log() {
  if [[ "$VERBOSE" != "" ]] ; then
    echo "[FEATURE-DB] $@" 1>&2
  fi
}

rails_check || exit 1
cli $@

#### TODO
# what to do about test db
## is it another schema or another db or what?
# robustness.  mainly pg perms.
# logging/verbose mode?
