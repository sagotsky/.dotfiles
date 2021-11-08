#!/bin/sh

red()  { while read -r line ; do echo -e "\e[0;31m$line\e[0m"; done; }
blue() { while read -r line ; do echo -e "\e[0;34m$line\e[0m"; done; }

work() { seq 20 -1 1; }
rest() { seq 10 -1 1; }

# this is what buffers the output.  to test it all at once, remove the sleep
output_timer() {
    while read -r line ; do
        echo -e "\e[1A\e[K$line" # rewrites previous line

        # echo "$round $line"
        sleep 1.0
    done
}

set_a() {
    round="$1"
    round=""
    work | output_timer | red
    rest | output_timer
}

set_b() {
    round="$1"
    work | output_timer | blue
    rest | output_timer
}

intro_message() { echo "Tabata starts in..." ; seq 5 -1 1 | output_timer; }

intro_message
seq 8 | while read -r round ; do
    clear
    echo -e "\n-- round $round --"
    echo
    set_a $round
    set_b $round

done
# while total <= 8
# seq 20 | blue
# seq 10 | red
# count

#

# source ~/.functions

# formatted_date() {
#     now="$(date)"
#     # why u no regex?
#     if [[ $now =~ ":1[0-9] " ]] ; then
#         blue $now
#     else
#         red $now
#     fi
# }

# export -f formatted_date
# export -f blue
# export -f red
# watch -n1 -c formatted_date
