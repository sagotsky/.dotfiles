#export PATH="$HOME/scripts/:$HOME/bin/:/usr/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/sbin:/bin:/usr/games:$PATH"

## Count columns at each cmd
shopt -s checkwinsize

# Disable term flow control.  ctrl-s no longer pauses.  Now bash's C-s can fire, which is hist forward
stty -ixon 2> /dev/null # pseudo ttys like ssh can't handle output
## Append debian chroot labels to prompt
if [ -z "$debian_chroot" -a -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

## screen session name labels for prompt.  shows name if specified with -S, otherwise pid.
if [ $STY ] ; then
  if [[ $STY =~ "${HOSTNAME}" ]] ; then
    # chop host
    SSN="[$(echo $STY | cut -f1 -d'.')]";
  else
    #chop pid
    SSN="[$(echo $STY | cut -f2 -d'.')]";
  fi
fi


## Tab complete sudo
#complete -cf sudo

## Adds hosts from .ssh/known_hosts to tab completion
complete -W "$(grep '^host \w' ~/.ssh/config | cut -f 2- -d ' ' | sort | uniq)" ssh

## variable colors (use sudo su -m to preserve colors)
## purple on prev command failure
## white on red for root
# color prefixes: 0 normal, 1 bold, 4 underline, 5 blink, 7 inverse
# colors: 31 red, 39 default, 37;41 white on red
DEFAULT='0;39m'
ps1color() {
  local color=$DEFAULT

  # read local colors
  if [ -f "$HOME/.color" ] ; then color=$(cat $HOME/.color) ; fi

  # error message - recolor to red
  if [ $1 -ne 0 ] && [ $1 -ne 130 ] ; then
    color="0;31m" ;
  fi

  # sudo active (assuming -n (ver 1.7+) supported) underline
  if [[ "$(sudo -n /bin/true 2>/dev/null ; echo $?)" == 0 ]] ; then
    color="4;37m" ;
  fi

  # root - red bgcolor, clobbers all other colors (including error)
  if [ ${UID} -eq 0 ]; then
    color='37;41m'
  fi
  echo $color;
}

function gitprompt() {
  branch=$( git branch 2>/dev/null | grep '*' | tr -d ' *')
  # check status.  diff color if dirty?
  if [[ $branch != "" ]] ; then
    url=$(git config --get remote.origin.url)
    color='0;33m' # changed yellow
    git diff --quiet && [ $(git ls-files --others --exclude-standard | wc -l) -eq 0 ] && \
      color="0;36m" # clean blue

    white='0;39m'
    echo "\n$url<\[\033[$color\]$branch\[\e[$white\]>"
  fi
}

## Prompt w/ date, screen session, color if root, deb chroot
MYPROMPT='<\T> \u@\h$SSN${debian_chroot:+.$debian_chroot}:\w'

## Add title for terms that support it
TITLE=""
case $TERM in
  xterm*|aterm|screen|rxvt*)
    TITLE="\[\033];\u@\h\007\]"
    ;;
esac



# haven't commited to adding alerts yet.  time reports that it's still slower than a good ole df -h.  le sigh.
PROMPT_COMMAND='COLOR=$(ps1color $?) ; BRANCH=$(gitprompt) ; history -a ; history -n ; PS1="${TITLE}\[\e[$COLOR\]$MYPROMPT\[\e[$DEFAULT\]$BRANCH\$ "'
#                                                                                                                                       git branch
#                                                                                                                         default color
#                                                                                                              user@host
#                                                                                                  color
#                                                                                   set title for xterm,screen
#                                                            update and reread history
#                                     get git info.  can be slow
#               set color
# PROMPT_COMMAND instead of PS1 to ensure that ps1color and gitprompt are executed

## Source some configs (.local files don't go in git)
if [[ "$-" == *i* ]] ; then  # only for interactive shells
  FILES=".alias .bashrc.local .alias.local .functions .bash_completion .bashrc.d/fd/fd.sh scripts/.ghiq.bash_completion.sh .shellrc"
  for FILE in $FILES ; do
    if [ -f "$HOME/$FILE" ] ; then
      source "$HOME/$FILE"
    fi ;
  done
fi

## long bash hist, append (not overwrite)
export HISTFILESIZE=50000
export HISTSIZE=50000
#export HISTCONTROL=erasedups # removed because it stopped me from counting frequently used commands
shopt -s histappend
set -o history # if this doesn't come last, other hist commands show up in hist


[ -f ~/.fzf.bash ] && source ~/.fzf.bash
