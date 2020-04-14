herbst-status.sh | init-polybar.sh &

function polybar_height() {
  cat ~/.polybarrc |
    grep '\(\[bar/\|height\)' | # only get bar declarations and heights
    grep $(hostname) -A 1 |     # current hostname and the height line after
    grep height |               # just the height line
    cut -f2 -d=                 # just the value

}
herbstclient pad 0 $(polybar_height) 0 0 0
# {
#   while : ; do

#     # echo "$line"
#     hc tag_status |
#       sed -e 's/[0-9]/⚫/g'
#     hc --wait
#   done
# } | init-polybar.sh &
