# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
if [[ -f /opt/homebrew/bin/brew ]] ; then
  source $(brew --prefix)/share/powerlevel10k/powerlevel10k.zsh-theme
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
for FILE in $(find "$ZSH/themes/" -name "*.zsh-theme") ; do source $FILE ; done

p10k_homebrew="/opt/homebrew/opt/powerlevel10k/powerlevel10k.zsh-theme"
[ -f "$p10k_homebrew" ] && source "$p10k_homebrew"

p10k_local_install="$HOME/source/powerlevel10k/powerlevel10k.zsh-theme"
[ -f "$p10k_local_install" ] && source "$p10k_local_install"

# ctrl-x-e
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# make delete work
bindkey '^[[3~' delete-char

# change cursor for tmux's paned environment
# [[ "$TERM" == 'screen-256color' && -x /usr/bin/xtermcontrol ]] && TERM=xterm xtermcontrol --cursor red


function shell_title() {
  print -Pn "\e]0;$1\a"
}

function preexec() {
  shell_title $*
}

if which keychain &>/dev/null && [[ "$SSH_AGENT_PID" == "" ]] || [[ "$SSH_AUTH_SOCK" == "" ]] ; then
  eval $(keychain --eval -q)
fi

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

PATH="$HOME/bin/:$PATH" # bin should outweigh .asdf, fight me.
# git clone --depth=1 https://github.com/romkatv/powerlevel10k.git .zsh//themes/powerlevel10k
#
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
#
## Source some configs (.local files don't go in git)
if [[ "$-" == *i* ]] ; then  # only for interactive shells
  for FILE in .{alias,functions}{,.$HOST} .shellrc .zshrc.$HOST; do
    [[ -e "$HOME/$FILE" ]] && source "$HOME/$FILE"
  done
fi

[[ -f /opt/homebrew/lib/asdf.sh ]] && source /opt/homebrew/lib/asdf.sh
[[ -f /opt/asdf-vm/asdf.sh ]] && source /opt/asdf-vm/asdf.sh
[[ -f /opt/homebrew/Cellar/asdf/0.14.0/libexec/asdf.sh ]] && source /opt/homebrew/Cellar/asdf/0.14.0/libexec/asdf.sh
true
