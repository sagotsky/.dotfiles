#!/bin/bash

SRC="fixme"
DEST="fixed"

function lightzoned() {
  exiftool -a -S -G0 "$1" | grep -i Zone &>/dev/null
}

function get_file_name() {
  zenity --file-selection
}

file=$(get_file_name)
if [[ -f "$file" ]] ; then
  convert "$file" --colorspace srgb -profile /usr/share/color/icc/colord/sRGB.icc "${file%%.jpg}-unyellowed.jpg"
fi


# find "$SRC" -type f -name '*.jpg' | while read img ; do
#   filename="${img##*/}"
#   path="${img%/*}"

#   mkdir -p "$DEST/$path"
#   echo $img
#   if lightzoned "$img" ; then
#     convert "$img" -colorspace srgb -profile /usr/share/color/icc/colord/sRGB.icc "$DEST/$path/$filename"
#   else
#     cp "$img" "$DEST/$path/$filename"
#   fi
# done

