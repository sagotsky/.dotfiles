#!/bin/bash

PROD_BASE=$1
DEV_BASE=$2
DIR=$3

export PHANTOMJS_HOME='/usr/share/doc/phantomjs/'

for dep in compare phantomjs ; do
  if [ "$(which $dep)" == '' ] ; then
    echo Could not find $dep.  Please make sure it\'s installed.
    exit 1
  fi
done

function url_to_image() {
  url=$1
  file=$2
  phantomjs $PHANTOMJS_HOME/examples/rasterize.js $url $file
}

while read url ; do
  #md5=$(echo $url | md5sum | cut -f 1 -d' ')
  sub=$(echo $url | tr -d ' ' | tr -c [:alnum:] _)
  rm -rf "$DIR/$sub"

  PROD_FILE="$DIR/${sub}/prod.png"
  DEV_FILE="$DIR/${sub}/dev.png"
  PROD_URL="${PROD_BASE}/${url}"
  DEV_URL="${DEV_BASE}/${url}"

  url_to_image "$PROD_URL" "$PROD_FILE"
  url_to_image "$DEV_URL" "$DEV_FILE"

  compare -metric MAE -compose Src "$PROD_FILE" "$DEV_FILE" "$DIR/$sub/diff.png" 2> "$DIR/$sub/stats.txt"
done

# this just accepts urls and outputs two files, then their diff.  diff stats into some statistics file (if missing, know that regen is needed)
# other program will manage list of urls to check.  



# other function to get urls, probably from url_alias

# compare in imagemagick to get a diff value, sort by those
# site shows pair of images.  hover to see the diff superimposed.
# from there, you can check off diff as desired.  send to github.  or trigger a rescan.