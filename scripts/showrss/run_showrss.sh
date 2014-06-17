#!/bin/bash

# path specified so that perl can see things that live in opt.
#PATH=$PATH:/opt/bin/
DIR="$HOME/scripts/showrss/"
cd $DIR
perl $DIR/showrss.pl -l $DIR/shows.log -o $HOME/Videos/rsstv/.watch
#/opt/bin/curl http://files.sagotsky.com/date/update.php > /dev/null

$DIR/showrss_gunzip.sh
#date > /tmp/showrss_last_run
