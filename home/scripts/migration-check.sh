#!/bin/bash

# Checks if your rails database has migrations that are not in your git
# checkout.  The intent is to run this after leaving a feature branch.  It'll
# remind you to go back to your feature branch to roll them back.

# If you use overcommit, you can do this automatically by putting a hook in
# your .overcommit.yml:
#
# PostCheckout:
#   CustomScript:
#     enabled: true
#     description: "Make sure recent migrations have files in the current branch"
#     required_executable: "migration-check.sh"
#
# To customize:
#   $LIMIT controls how many recent migrations will be checked.
#   psql_cmd is a function that connects to your db.  It should be able to run
#     queries via STDIN

LIMIT=16 # how many migrations to test?

gitroot() {
  git rev-parse --show-toplevel
}

migration_exists() {
  version=$1
  find $(gitroot)/db/migrate -name "${version}*" | grep . &>/dev/null
}

check_migrations() {
  current_migration_versions | while read version ; do
    if ! migration_exists $version ; then
      echo "Migration $version is live but there is no file.  Did it get left behind in a feature branch?"
    fi
  done
}

current_migration_versions() {
  echo "select * from schema_migrations order by version desc limit $LIMIT" |
     psql_cmd |
     egrep " [0-9]+$"
}

# this might need to be a variable so non-ezcater apps can use it
psql_cmd() {
  $(gitroot)/docker/psql 2>/dev/null
}

migration_messages="$(check_migrations)"
if [[ "$migration_messages" != "" ]] ; then
  echo "$migration_messages"
  exit 1
fi
