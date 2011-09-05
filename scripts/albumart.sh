#!/bin/bash

# albumart.sh
# Finds album artwork for a list of music folders
# FOlders are likely to be band_name/album_name format
#   folders may also include year, release group, format, cd1/cd2, etc
# should ignore folders with existing folder.jpg

# usage:
# find MusicFolder/ -type d | albumart.sh

DISCOGS_API_KEY="90aa12d8d7"

function hasArt() {
    albumart="$1/folder.jpg"
    if [ -f "$albumart" ] ; then
	return 1;
    else 
	return 0;
    fi
}

#it'd be awesome if we could search once for artist then grab all art...
# http://www.discogs.com/artist/blind+guardian?f=xml&api_key=90aa12d8d7
function getArt() {
    album=` echo $1 | sed -e 's/^.*\/\(.*\)$/\1/' `
    band=` echo $1 | sed -e 's/^.*\/\(.*\)\/.*$/\1/' `
    #also grab previous group in case path is /band/album/disc1
    #then prev becomes album
    prev=` echo $1 | sed -e 's/^.*\/\(.*\)\/.*\/.*$/\1/' `

}

while read path
do
    hasArt "$path"
    if [ $? -eq 1 ] ; then
	echo "" #$path has a folder.jpg already ... skipping"
    else 
	getArt "$path"
	echo "$path"
    fi
done

