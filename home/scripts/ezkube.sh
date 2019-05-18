#!/usr/bin/env bash

APPLICATION=$1
ROLE=$2
shift
shift
CMD=${@:-/bin/bash}

kubectl config current-context
echo

LIST=""
if [[ "$APPLICATION" != "" ]] ; then
  LIST="-l application=$APPLICATION"
fi

if [[ "$ROLE" != "" ]] ; then
  LIST="$LIST,role=$ROLE"
fi

PODS="$(kubectl get pods $LIST | grep -v STATUS)"

if [[ $(echo "$PODS" | wc -l) == "1" ]] ; then
  POD="$PODS"
else
  POD=$(echo "$PODS" | fzf)
fi

PODNAME=$(echo $POD | cut -f1 -d' ')
kubectl exec -it $PODNAME -- $CMD
