#!/usr/bin/env bash

set -eo pipefail

CONTEXT="${CONTEXT}"
NAMESPACE="${NAMESPACE:-}"

APPLICATION=$1
ROLE=$2
shift
shift
CMD="${@:-/bin/bash}"

LIST=""
if [[ "$APPLICATION" != "" ]] ; then
  LIST="-l application=$APPLICATION"
fi

if [[ "$ROLE" != "" ]] ; then
  LIST="$LIST,role=$ROLE"
fi

PODS="$(kubectl $NAMESPACE get pods $LIST | grep -v STATUS | grep Running)"

if [[ $(echo "$PODS" | wc -l) == "1" ]] ; then
  POD="$PODS"
else
  POD=$(echo "$PODS" | fzf)
fi

PODNAME=$(echo $POD | cut -f1 -d' ')
kubectl $NAMESPACE exec -it $PODNAME -- $CMD
