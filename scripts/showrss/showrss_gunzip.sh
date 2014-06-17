#!/bin/bash

dir="$HOME/Videos/rsstv"
#for file in $(find "$dir" -type f -printf "%f\n") ; do
  #type=$(file "$dir/$file" | cut -f 2 -d:)
  #if [[ "$type" == *gzip* ]] ; then
    #mv $dir/$file /tmp/$file.gz
    #gunzip /tmp/$file.gz
    #mv /tmp/$file $dir/
  #fi
#done
find $dir -name '*.torrent.torrent' | while read file ; do
  mv $file ${file%%.torrent}.gz
  gunzip ${file%%.torrent}.gz
  #rm ${file%%.torrent}.gz
done
