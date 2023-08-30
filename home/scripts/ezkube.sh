#!/usr/bin/env bash

set -eo pipefail

CONTEXT="--context ${CONTEXT}"
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

PODS="$(kubectl $CONTEXT $NAMESPACE get pods $LIST | grep -v STATUS | grep Running)"

if [[ $(echo "$PODS" | wc -l) == "1" ]] ; then
  POD="$PODS"
else
  POD=$(echo "$PODS" | fzf)
fi

PODNAME=$(echo $POD | cut -f1 -d' ')
echo "try k9s --context $CONTEXT --namespace $NAMESPACE"
for n in 1 2 3 4 5 ; do >&2 echo -n "." ; sleep 1 ; done ; echo
kubectl $CONTEXT $NAMESPACE exec -it $PODNAME -- $CMD
