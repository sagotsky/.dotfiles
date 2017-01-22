#!/bin/sh

# start and kill a blank dzen so xmonad restores the right dock size.  this is dumb.
echo '' | 
  dzen2 -fn termsyn-8 -xs 1 -p 1 -dock -w 1 & 
sleep .1 
kill $!
