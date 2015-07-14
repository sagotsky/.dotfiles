#!/bin/sh


amixer sset Master 4%- > /dev/null
touch /dev/snd/controlC0 
