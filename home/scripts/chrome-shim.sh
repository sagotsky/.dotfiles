#!/bin/sh

# This is a stupid shell trick.  Symlink it to a url.  It'll open a browser with that url.  Now rofi can lanuch a few sites, e.g. `ln -s $HOME/scripts/chrome-shim.sh ~/bin/hulu.com`
url="$(basename $0)"
chrome $url
