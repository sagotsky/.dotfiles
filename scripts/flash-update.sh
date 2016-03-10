#!/bin/bash

FLASH_URL="https://get.adobe.com/flashplayer/download/?installer=Flash_Player_11.2_for_other_Linux_(.tar.gz)_64-bit&standalone=1"
TMP="$(mktemp -d)"

function tarball_url() {
  curl -s "$FLASH_URL" | 
    grep https://fpdownload | 
    sed -e "s/.*'\(.*\)'.*/\1/"
}

function download_and_unpack() {
  curl -s -k "$(tarball_url)" | tar -xzf - -C "$TMP"
}

function patch_fullscreen() {
  sed -i s/_NET_ACTIVE_WINDOW/_ZZZ_ACTIVE_WINDOW/ "$TMP/libflashplayer.so"
}

function install_flash() {
  sudo mv "$TMP/libflashplayer.so" /usr/lib/mozilla/plugins/
}

download_and_unpack
patch_fullscreen
install_flash
