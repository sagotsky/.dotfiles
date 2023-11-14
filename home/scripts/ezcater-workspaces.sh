#!/bin/bash


systemd_tmux() {
  systemd-run --scope --user -q tmux -2 "$@"
}

function tmux-up {
  DIR=$1
  NAME="${2:-$(basename $DIR)}" # or $2?

  if systemd_tmux has-session -t $NAME &>/dev/null ; then
    echo "$NAME already running"
  else
    cd $DIR

    # this lets us kill the first window, then replace it
    systemd_tmux set -g remain-on-exit on
    systemd_tmux -2 new-session -d -s "$NAME" #exit
    systemd_tmux set -g remain-on-exit off

    while read line ; do
      _handle_line $line
    done

    systemd_tmux kill-pane
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
  systemd_tmux new-window -t $NAME
}

# always respawn on last pane and prep the next pane.
function window {
  systemd_tmux respawn-pane -t $NAME -k $@
}

function vsplit {
  # use different strategy than windows.  append panes, then kill first one.  i don't care if they lose the 0 index.
  echo $@ | while IFS= read -r line ; do
    xargs printf '%s\n' <<<"$line" | while read cmd ; do
      systemd_tmux split-window -d -h "$cmd ; zsh"
    done
  done
  systemd_tmux kill-pane -t 0 # -d left it on original pane
}

function hsplit {
  # use different strategy than windows.  append panes, then kill first one.  i don't care if they lose the 0 index.
  echo $@ | while IFS= read -r line ; do
    xargs printf '%s\n' <<<"$line" | while read cmd ; do
      systemd_tmux split-window -d -v "$cmd ; zsh"
    done
  done
  systemd_tmux kill-pane -t 0 # -d left it on original pane
}


# example to test drive splits
# todo:
## make splits work
## make splits work with args.  don't care if they're quoted
# tmux-up /tmp <<EOF
#   vsplit "watch\ date" htop
# EOF

# exit

tmux-up ~/repos/avro-schema-registry <<EOF
  docker/start
EOF

tmux-up ~/repos/direct-connect-rails <<EOF
  docker/start
EOF

tmux-up ~/repos/omnichannel-rails <<EOF
  docker/start
EOF

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
  vsplit docker/start-selenium "sleep 15 ; docker attach `docker ps | grep ez-rails-web | awk '{print $1}'` --detach-keys 'ctrl-c'"
EOF

tmux-up ~/repos/payouts-rails <<EOF
  cd ~/repos/eztilt
  # vsplit docker/start "sleep 15 ; docker attach `docker ps | grep payouts-rails-web | awk '{print $1}'` --detach-keys 'ctrl-c'"
EOF

# tmux-up ~/repos/ez-rails <<EOF
#   docker-compose up ezrails-db
#   foreman start
#   bin/rails s -b 0.0.0.0
# EOF

# override default name with second arg
# tmux-up ~ autossh <<EOF
#   ~/scripts/screen-ssh-tunnel-omelette.sh
# EOF

# tmux-up ~/repos/developer-handbook dev-handbook <<EOF
#   docker-compose up
# EOF

# tmux-up ~/repos/engineering-rfcs rfc<<EOF
#   zsh
# EOF

# tmux-up ~/repos/menus-graphql-prototype gql <<EOF
#   hsplit zsh bin/start
# EOF
