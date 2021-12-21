#!/usr/bin/env bash

there-can-be-only-one.sh
# killall dzen2

TIME=${1:-60}
SELF=$(basename "$0")
# FONT=hack-42
FONT="Source Code Pro Semibold-12"
# FONT="Meslo-42"

sec_to_min() {
    min=$(($1 / 60))
    sec=$(($1 % 60))
    printf "%02d:%02d" $min $sec
    # echo "$min:$sec"
}

# 24 DeepSkyBlue4
dzen_format() {
    txt="$1"
    if [[ "$txt" =~ 0:0. ]] ; then
        fg="red"
        bg="black"
    else
        fg="green"
        bg="black"
    fi

    if [[ "$txt" == "" ]] ; then
        fg="Grey39"
        bg="Grey11"
        txt="--:--"
    fi
    pre="^fg($bg)^fg()"
    post="^fg($bg)^fg()"
    echo "${pre}^fg($fg)^bg($bg)  $txt  ^fg()^bg()${post}"
}

blink() {
    spaces="$(printf '% 2000s' ' ')"

    for n in $( seq 10 ) ; do
        echo "^bg(white)$spaces"
        sleep 0.05
        echo "^bg(black)$spaces"
        sleep 0.05
    done
}

alarm() {
    [[ "$?" == "0" ]] && mpv --volume=60 /usr/share/sounds/freedesktop/stereo/complete.oga &>/dev/null &
}

(
    seq "$TIME" -1 0 | while read -r n ; do
        dzen_format "$(sec_to_min $n)"
        sleep 1
    done

    alarm
    blink

    dzen_format
) | dzen2 -fn "$FONT" -bg Grey15 -p -e "button3=exec:killall -9 dzen2;button1=exec:$SELF $TIME" -xs 1 &
