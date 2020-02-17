# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# TODO: try zsh-async

# zmodload zsh/zprof

# Set up the prompt
setopt NO_HUP   # don\'t kill running processes when exiting the shell

# autoload -Uz promptinit

setopt histignorealldups sharehistory

setopt PROMPT_SUBST

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/.history_zsh

fpath=(~/.zsh/completion ~/.zsh/functions $fpath)
autoload $(ls ~/.zsh/functions)

# Use modern completion system
autoload -Uz compinit
find ~/.zcompdump -mtime +2 -delete &>/dev/null # delete old zcopmdump
compinit -C # 30ms
# removing the -C slows this down but recalculates some stuff.  not obvious why i ever need to do that


zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
which dircolors &>/dev/null && eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# fix alt-backsp
autoload -U select-word-style
select-word-style bash

stty stop undef &>/dev/null     # reclaim ctrl-s as forward search

# try to load selective plugins from oh-my-zsh
ZSH="$HOME/.zsh/"
for FILE ($ZSH/plugins/**/*sh) ; do source $FILE ; done
for FILE ($ZSH/themes/**/*.zsh-theme) ; do source $FILE ; done
#update_current_git_vars # this is the slow 30ms

# ctrl-x-e
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# make delete work
bindkey '^[[3~' delete-char

# change cursor for tmux's paned environment
[[ "$TERM" == 'screen-256color' && -x /usr/bin/xtermcontrol ]] && TERM=xterm xtermcontrol --cursor red

function prompt_color {
  STATUS="$?"
  COLOR="%{$reset_color%}"

  # this is getting echoed...
  [[ "$STATUS" -gt 0 ]] &&  COLOR="%F{red}"     # red on error
  [[ "$(sudo -n /bin/true 2> /dev/null ; echo $?)" == 0 ]] && COLOR="$COLOR%U" # underline on sudo

  echo $COLOR
}

function reset_color {
  # echo "%{$reset_color%}"
  echo "%F{white}%u%b"
}

# commands that take more than 30s to complete get a visual bell
# might get obnoxious with long running apps (vim)
# might get weird with background jobs
function preexec() {
  _SECONDS_BEFORE=$SECONDS
}

function precmd() {
  if [ ! -z $_SECONDS_BEFORE ] ; then
    [ $(( $SECONDS - ${_SECONDS_BEFORE} )) -gt 15 ] && echo -e "\a"
  fi
}
function shell_title() {
  print -Pn "\e]0;$1\a"
}
# shell_title $ZSH_NAME

PROMPT="%n@%m:%~"                       # user@host:~
PROMPT="<%*> $PROMPT"                   # <hh:mm:ss>
PROMPT="\$(prompt_color)$PROMPT\$(reset_color) \$(git_super_status)$ \$(reset_color)" # git info
# TODO - bell if this term's owner isn't on current screen.  long running procs will always bell when done.  xprop could get ugly (xterm -> tmux -> vim), but maybe setting a global var on init would be cleaner?


# TODO: add a bunch more of these
ZSH_THEME_GIT_PROMPT_BRANCH="%F{cyan}%B"
# maybe add functions escaped and then sed them up into $()

# Show pretty background jobs list unless empty
function _rprompt_jobs {
  jobs | sed -e "s/ .*  //" | tr -d "\n" | tr "]" ":" | tr "[" " "
}
function _rprompt_git_stash {
  git status 2>/dev/null && echo "|$(git stash list | tail -n 1  | cut -f1 -d:)"
}
RPROMPT='%(1j.$( _rprompt_jobs ).)' #$( _rprompt_git_stash )'
#RPROMPT="%F{54}$RPROMPT%f"

## Source some configs (.local files don't go in git)
if [[ "$-" == *i* ]] ; then  # only for interactive shells
  for FILE in .{alias,functions}{,.$HOST} .shellrc .zshrc.$HOST; do
    [[ -e "$HOME/$FILE" ]] && source "$HOME/$FILE"
  done
fi

# source /opt/asdf-vm/asdf.sh

# async_init

# ~/etc/zsh/lib% cat k8s.zsh
# function load_kubectl_comp() {
#   if [ $commands[kubectl] ]; then
#     source <(kubectl completion zsh)
#   fi
# }
# async_start_worker kubectl_comp_worker -n
# async_register_callback kubectl_comp_worker load_kubectl_comp
# async_job kubectl_comp_worker sleep 0.1
#
# nvm.sh adds ~400ms to shell startup.  lazy load it instead.
# export NVM_DIR="/home/sagotsky/.nvm"
# alias init_nvm="[ -s '$NVM_DIR/nvm.sh' ] && . '$NVM_DIR/nvm.sh'"
# alias node='unalias node ; unalias npm ; init_nvm ; node $@'
# alias npm='unalias node ; unalias npm ; init_nvm ; npm $@'

# slow things
# run "zsh -i -c exit" to profile
# normal startup: ~190
#
# git status 222
# minus nvm: no real change
# minus dot loop source: 90!
## shellrc has most of this.  hey look, it's rbenv!
## ditching rbenv gets us down to 115
# update_current_git_vars: 150
# compinit: 140
# in plm: 800!!!  probably git status
### todo: gitstatus daemon.  it outputs every time .git changes (or some other watch condition).  prompt is responsible for reading in output or starting up daemon.
# - all: 14!

# ctrl-kj don't work in tmux
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
#stop_profiling
# zprof

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
