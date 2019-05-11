#!/bin/bash

# quick and dirty gmail checker.  positive exit status indicates new mail.

USER=$1
URL="https://mail.google.com/mail/u/$USER/feed/atom/"
curl -s -b ~/.gmail-cookies.txt $URL | grep '<entry>' #&>/dev/null
