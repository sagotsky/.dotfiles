#!/bin/sh

# convert to avi.
# get resolution first

FILE=$(zenity --title='Video File' --file-selection)
RES=$(zenity --title 'Resolution' --entry --text 'What size should the video be?' --entry-text=640x480)
FORMAT=$(zenity --list --radiolist --column '' --column 'Format' TRUE 'avi' FALSE mpeg FALSE mpg)

ffmpeg -i $FILE -s $RES ${FILE%.*}_.$FORMAT
