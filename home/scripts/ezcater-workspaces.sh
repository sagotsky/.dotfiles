#!/bin/bash

#alt name: tmux-compose?
function tmux-up {
  DIR=$1
  NAME="$(basename $DIR)" # or $2?

  if tmux has-session -t name &>/dev/null ; then
    echo "$NAME already running"
  else 
    cd $DIR

    # this lets us kill the first window, then replace it
    tmux set -g remain-on-exit on
    tmux -2 new-session -d -s "$NAME" #exit
    tmux set -g remain-on-exit off

    tmux ls
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

  $TMUX_CMD $@
  # always leave next window ready for next command
  tmux new-window -t $NAME 
}

# always respawn on last pane and prep the next pane. 
function window {
  tmux respawn-pane -t $NAME -k $@
}



# bug: still can't use args in splits
function vsplit {
  for app in $@ ; do
    tmux split-window -d -h $app 
  done
  # tmux kill-pane # -d left it on original pane
}

function hsplit {
  for app in $@ ; do
    tmux split-window -d -v $app 
  done
  # tmux kill-pane # -d left it on original pane
}


# example to test drive splits
# todo: 
## make splits work
## make splits work with args.  don't care if they're quoted
# tmux-up /tmp <<EOF
#   htop
#   hsplit 'watch date' htop
#   watch date
# EOF

tmux-up ~/repos/avro-schema-registry <<EOF
  docker/start
EOF

tmux-up ~/repos/kafka-docker <<EOF
  docker/start
EOF

tmux-up ~/repos/authentication-rails <<EOF
  docker/start
EOF

tmux-up ~/repos/pos-rails <<EOF
  docker/start
EOF

tmux-up ~/repos/ez-rails <<EOF
  bin/rails s
  foreman start
EOF

# tmux-up ~/repos/ez-rails <<EOF
#   vsplit foreman "bin/rails s"
#   window zsh
# EOF


# workspaces="$(cat <<EOF
#   ez-rails 'bin/rails s'
#   kafka-docker
# EOF
# )"

# echo "$workspaces"

# avro first?
# authentication-rails
# for workspace in ez-rails kafka-docker pos-rails ; do
# for workspace in ez-rails kafka-docker pos-rails ; do
#   tmux ls | grep -q "$workspace" || (
#     cd ~/repos/workspace
#     tmux -2 new-session -d -s "$workspace"
#   )
# done
