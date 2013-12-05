#!/bin/sh

# wallpaper switcher.  no arg gives random.  new or old arg does
# 1 file newer or older than current.  current is determined by
# a symlink - if there is none, run random to set it up

DIR="$HOME/.wallpaper"
LINK="$HOME/.wallpaper.png"
CURRENT=$(readlink $LINK)

case "$1" in
  new|old)
    WALLPAPERS=($(ls -t $DIR/*.{png,jpg,jpeg}))
    for i in "${!WALLPAPERS[@]}" ; do
      if [[ ${WALLPAPERS[$i]} == $CURRENT ]] ; then
        case "$1" in
          'new')
            IMG="${WALLPAPERS[(( $i - 1))]}"
            break
            ;;
          'old')
            IMG="${WALLPAPERS[(( $i + 1))]}"
            break
            ;;
        esac
      fi
    done
    ;;

  current)
    IMG="$CURRENT"
    ;;

  *)
    if [[ $CURRENT == '' || "$1" != "new" && "$1" != 'old' ]] ; then
      IMG=$( find "$DIR/" | shuf -n1 )
    fi
    ;;
esac

notify-send M-S-b "${IMG##*/}" &
feh --bg-fill --no-xinerama "$IMG"
if [[ "$LINK" != "" ]] ; then
  ln -fs "$IMG" "$LINK"
fi

# also make a color palette from the new wallpaper
which image_to_palette.sh && image_to_palette.sh $LINK > $DIR/.palette.txt

