#!/bin/sh

# Rip an iso of a mounted CD

#title="`head -n 1 /dev/cdrom | awk  '{print $2} '`"
title="`head -n 1 /dev/cdrom | sed -e 's/\S*\s*//' -e 's/\s\{3,20\}/\n/' | head -n 1`"

echo "Ripping $title..."
dd if=/dev/cdrom bs=512 | pv | dd of="$title.iso" 
echo "Ripped!"
echo ""
eject /dev/cdrom
