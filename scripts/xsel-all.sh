#!/bin/sh

cat | xsel -p
xsel -p | xsel -s
xsel -s | xsel -b
xsel --keep
