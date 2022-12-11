#!/bin/sh

amixer -D pulse sset Master toggle #> /dev/null
amixer -D pulse -c 3 sset PCM toggle #> /dev/null

# touch /dev/snd/controlC0
