#!/bin/bash -e


# ssh into a work stage server by name or list all active stage servers
if [[ "$#" != "0" ]] ; then
  HOST=$(curlpw.sh -s http://devdash.stage.patientslikeme.com/ |
    grep ">$1<" -A 9 |
    tail -n 1 | 
    sed -e 's/.*"ssh:\/\/\(.*\)".*/\1/')
  shift
fi 

if [[ "$HOST" == "" ]] ; then
  curlpw.sh -s http://devdash.stage.patientslikeme.com/ | 
    grep branches/refresh | 
    perl -pe 's|.*branch=(.*?)".*|\1|'
else 
  BG=$(xtermcontrol --get-bg)
  xtermcontrol --bg '#123'
  ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null $@ $HOST 
  xtermcontrol --bg $BG
fi 

