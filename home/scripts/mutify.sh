#!/bin/bash

# mute spotify during ads.  maybe play something else instead.
# instructions: run this following notifications.  dunst scripts work well.

spotify_window_title() {
  xprop -name 'Spotify Free - Linux Preview' WM_ICON_NAME |
    sed -e 's/.*"\(.*\)".*/\1/' -e 's/"$//g'
}

spotify_track_meta() {
  META="$(qdbus org.mpris.MediaPlayer2.spotify / org.freedesktop.MediaPlayer2.Metadata)"
  ARTIST="$(echo "$META" | grep xesam:artist | cut -f 2- -d' ')"
  ALBUM="$(echo "$META" | grep xesam:album | cut -f 2- -d' ')"
  TITLE="$(echo "$META" | grep xesam:title | cut -f 2- -d' ')"

  echo "Spotify - $ARTIST – $TITLE"
}

playing_song() {
  [[ *"$(spotify_window_title)"* == *"$(spotify_track_meta)"* ]]
}

mute() {
  echo 'mute'
  amixer sset Master mute > /dev/null
  # pacmd to target spotify and only spotify
}

unmute() {
  echo 'unmute'
  amixer sset Master unmute > /dev/null
}

playing() {
  [[ 'Playing' == $(qdbus org.mpris.MediaPlayer2.spotify / org.freedesktop.MediaPlayer2.PlaybackStatus) ]]
}

# daemon mode.   xprop -spy
# spotify doesn't notify you on a new ad playing
#pacmd?

watch_spotify_window() {
  xprop -spy -name 'Spotify Free - Linux Preview' WM_ICON_NAME
}

# disable on premium
watch_spotify_window | while read event ; do
  if playing_song ; then
    unmute
  else 
    playing && mute
  fi
done



