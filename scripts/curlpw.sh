#!/bin/bash

# Wrapper for curl.  Use this to store basic auth credentials in another file per domain.  Keep that file out of VCS.

DOMAIN="$(echo "$@" | perl -pe 's|.*(http://.*?/).*|\1|')"
OPTIONS="$(grep ^$DOMAIN ~/.curlpw | cut -f 2- -d' ')"
curl $OPTIONS $@
