#!/bin/bash

#alt name: tmux-compose?
function tmux-up {
  DIR=$1
  NAME="${2:-$(basename $DIR)}" # or $2?

  if tmux has-session -t $NAME &>/dev/null ; then
    echo "$NAME already running"
  else
    cd $DIR

    # this lets us kill the first window, then replace it
    tmux set -g remain-on-exit on
    tmux -2 new-session -d -s "$NAME" #exit
    tmux set -g remain-on-exit off

    while read line ; do
      _handle_line $line
    done

    tmux kill-pane
  fi
}


function _handle_line {
  TMUX_CMD=$1

  case $TMUX_CMD in
    hsplit|hs)
      TMUX_CMD='hsplit'
      shift
      ;;
    vsplit|vs)
      TMUX_CMD='vsplit'
      shift
      ;;
    window|w)
      TMUX_CMD='window'
      shift
      ;;
    *)
      TMUX_CMD='window'
      # don't shift
  esac

  $TMUX_CMD "$@"

  # always leave next window ready for next command
  tmux new-window -t $NAME
}

# always respawn on last pane and prep the next pane.
function window {
  tmux respawn-pane -t $NAME -k $@
}

function vsplit {
  # use different strategy than windows.  append panes, then kill first one.  i don't care if they lose the 0 index.
  echo $@ | while IFS= read -r line ; do
    xargs printf '%s\n' <<<"$line" | while read cmd ; do
      tmux split-window -d -h "$cmd ; zsh"
    done
  done
  tmux kill-pane -t 0 # -d left it on original pane
}

function hsplit {
  # use different strategy than windows.  append panes, then kill first one.  i don't care if they lose the 0 index.
  echo $@ | while IFS= read -r line ; do
    xargs printf '%s\n' <<<"$line" | while read cmd ; do
      tmux split-window -d -v "$cmd ; zsh"
    done
  done
  tmux kill-pane -t 0 # -d left it on original pane
}


# example to test drive splits
# todo:
## make splits work
## make splits work with args.  don't care if they're quoted
# tmux-up /tmp <<EOF
#   vsplit "watch\ date" htop
# EOF

# exit

# tmux-up ~/repos/avro-schema-registry <<EOF
#   docker/start
# EOF

# tmux-up ~/repos/kafka-docker <<EOF
#   docker/start
# EOF

# tmux-up ~/repos/ezcater-identity identity <<EOF
#   docker/start
# EOF

# tmux-up ~/repos/authentication-rails <<EOF
#   docker/start
# EOF

tmux-up ~/repos/ez-rails <<EOF
  vsplit docker/start "sleep 15 ; docker attach `docker ps | grep ez-rails-web | awk '{print $1}'` --detach-keys 'ctrl-c'"
EOF

# tmux-up ~/repos/ez-rails <<EOF
#   docker-compose up ezrails-db
#   foreman start
#   bin/rails s -b 0.0.0.0
# EOF

# override default name with second arg
tmux-up ~ autossh <<EOF
  ~/scripts/screen-ssh-tunnel-omelette.sh
EOF
