#!/bin/sh

# yoinks cover art from amarok and copies to correct path
# amarok must be running
#
# http://twistylife.blogspot.com/2008/01/cd-cover-art-in-amarok.html

dcop amarok player enableOSD false
dcop amarok player enableRandomMode false
dcop amarok player enableRepeatPlaylist false
dcop amarok player enableRepeatTrack false
dcop amarok player mute

while [ $(dcop amarok player status) -gt 0 ]  ; do
    ALBUM=$(dcop amarok player album)
    NOW=$(dcop amarok player nowPlaying)
    TRACKPATH=$(dcop amarok player path)
    COVER=$(dcop amarok player coverImage)
    D=$(dirname "$TRACKPATH")

    dcop amarok player next

    if [ -f "$D/folder.jpg" ] ; then 
#	echo "*** $D has folder.jpg"
	echo -n "."
    else
	case $COVER in 
	    *nocover.png)
		echo "***** $D has no kde cover!"
		;;
	    *)
		convert "$COVER" "$D/folder.jpg"
		echo ""
		echo "copying to $D"
		echo ""
		;;
	    esac
    fi
done    


dcop amarok player enableOSD true
echo ""
echo "All done!"
echo ""