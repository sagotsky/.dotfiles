#!/bin/sh

usage() {
    cat <<EOF
Usage:
    $(basename $0) daemon - starts a dunst daemon that watches when it pauses dunst.  Notifications formatted for yambar.
    $(basename $0) click -  toggle dunst and trigger the daemon to emit a notification

EOF
}

yambar_dunst_is_paused() {
    echo -e "dunst_is_paused|bool|$(dunstctl is-paused)\n"
}

click_hook() {
    dunstctl set-paused toggle
    yambar_dunst_is_paused
}

toggle_dunst() {
    pgrep -f "$(basename $0)" | grep -v $$ | xargs kill -s USR1
}

case $1 in
    "daemon")
        yambar_dunst_is_paused
        trap click_hook USR1
        while : ; do
            sleep 3m &
            sleep_pid="$!"
            wait $sleep_pid
            kill $sleep_pid
        done
        ;;
    "click")
        toggle_dunst
        ;;
    *)
        usage

esac
