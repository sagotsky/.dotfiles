#!/usr/bin/python
# Little script to depack Matroska file, and repack them
# in a AVI + subtitle format.

import sys
import os

def message(msg):
    print "=" * 78
    print "= %s" % msg
    print "=" * 78

def usage():
    print "Mastroka repacker script"
    print "  Usage: "+sys.argv[0]+ " filename"

if __name__ == "__main__":
    if len(sys.argv) < 2:
        usage()
    else:
        filename = sys.argv[1]
        basename = filename[:-4]
    
        message("Unpacking file: %s" % filename)
        os.system("mkvextract tracks %s 1:temp_video.avi 2:temp_audio.ogg 3:%s.srt" % (filename,basename) )

        message("Repacking file: %s.avi" % basename)
        os.system("ffmpeg -i temp_audio.ogg  -i temp_video.avi  -vcodec copy  %s.avi" % (basename) )

        message("Cleaning files")
        os.system("rm temp_video.avi temp_audio.ogg")
