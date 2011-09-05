#!/bin/bash

# Opens shell.fm, filling out url depending on cli switches

USER='valadil'
BIN=$(which shell-fm 2>/dev/null || echo "$HOME/disk/source/shell-fm-0.5/source/shell-fm")
STATION="lastfm://user/$USER/personal"

function usage() { sed -e 's/^   //' <<EndUsage

    lastfm.sh usage:
    -? or -h      display this help message
    -a ARTIST     play artist radio
    -g GENRE      play genre radio
    -u USER       play last.fm user radio
    -n USER       play last.fm user neighbour radio

EndUsage
  exit 1
}


while getopts "?hu:a:g:n:" flag ; 
  do
  case $flag in
      '?' ) 
	  usage
	  ;;

      'h' ) 
	  usage
	  ;;

      'a' ) 
	  STATION="lastfm://artist/$OPTARG/similarartists" 
	  ;;

      'g' ) 
	  STATION="lastfm://globaltags/$OPTARG"
	  ;;

      'n' )
	  STATION="lastfm://user/$OPTARG/neighbours"
          ;;

      'u' )
	  STATION="lastfm://user/$OPTARG/personal"
          ;;
    esac
done


$BIN $STATION

