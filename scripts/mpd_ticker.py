#!/usr/bin/python

# MPD client
# Watches MPD for changes, returns text summaries of changes, as specified by user
# Useful for updating scripts of song changes, without relying on polling
# Needs mpd.idle, mpd 0.14 or higher

import mpd
import sys
from string import Template

# user vars
host = "127.0.0.1"
port = 6600
template = Template('$artist - $title ($rating)')
stickers = ['rating'] # also fetches stickers.  can be referenced in template

# connect to mpd
client = mpd.MPDClient()
client.connect(host, port)
fields = ["album", "title", "track", "artist", "genre", "file", "time"];

# wait loop
while client.idle():
# need to check if status is a new song or not
    status = client.status()
    song = client.currentsong()

    # prep vars for template
    replacements = dict()
    for f in fields:
        replacements[f] = song[f]

    for s in stickers:
        try:
            sticker = client.sticker("get", "song", song['file'], s)[0]
            sticker = sticker.strip(s+"=")
            replacements[s] = sticker
            break
        except mpd.CommandError:
            replacements[s] = ""

    # replace and print from template
    print template.safe_substitute(replacements)
    sys.stdout.flush()

client.disconnect()
print "EOF\n";

