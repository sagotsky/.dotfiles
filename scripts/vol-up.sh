#!/bin/sh

amixer sset Master 4%+ unmute > /dev/null
touch /dev/snd/controlC0 

