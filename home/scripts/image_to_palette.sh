#!/bin/sh

# scrapes medium palette from cssdrive.
# yes, this was quicker than using an api.  couldn't find a free one that did what I wanted.
# requires curl, imagemagick, https://gist.github.com/MicahElliott/719710

# uploads thumbnail since limit is 1mb
# gets the medium palette
# strips everything but a color from each line
URL='http://www.cssdrive.com/imagepalette/index.php'
convert -resize 100x "$@" /tmp/thumbnail.png
curl -s --form "uploadFile=@/tmp/thumbnail.png" $URL |
  grep 'Complete Color Palette' -A50 |
  grep -i '#[0-9a-f]\{6\}' |
  sed -e 's/.*\(#[0-9a-f]\{6\}\).*/\1/i' 


