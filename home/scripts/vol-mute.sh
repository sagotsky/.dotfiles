#!/bin/sh

amixer -D pulse sset Master toggle #> /dev/null
amixer -D pulse -c 2 sset PCM toggle #> /dev/null

# touch /dev/snd/controlC0
