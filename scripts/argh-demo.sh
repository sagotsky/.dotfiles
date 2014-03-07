#!/bin/bash

# 

eval "$(argh.sh $@)"

#- hashdash comments will be displayed inline with help
PORT="${PORT:-8080}" #- returns $PORT if set, or defaults to returning 8080
TEST="${TEST:-asdf}" #- just a test arg
DEF="${DEF:-default string}" #- just use the default

echo $PORT
echo $TEST
echo $DEF

