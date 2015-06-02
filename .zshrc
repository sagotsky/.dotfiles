# Set up the prompt
setopt NO_HUP   # don't kill running processes when exiting the shell

# autoload -Uz promptinit

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/.history_zsh

fpath=(~/.zsh/completion $fpath)

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
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


# try to load selective plugins from oh-my-zsh
ZSH="$HOME/.zsh/"
for FILE ($ZSH/plugins/**/*sh) ; do source $FILE ; done
update_current_git_vars

# ctrl-x-e
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# change cursor for tmux's paned environment
[[ "$TERM" == 'screen-256color' && -x /usr/bin/xtermcontrol ]] && TERM=xterm xtermcontrol --cursor red

function prompt_color {
  STATUS="$?"
  COLOR="%{$reset_color%}"

  [[ "$STATUS" -gt 0 ]] &&  COLOR="%{$fg[red]%}"     # red on error
  [[ "$(sudo -n /bin/true 2> /dev/null ; echo $?)" == 0 ]] && COLOR="$COLOR%U" # underline on sudo
  [[ "${UID}" -eq "0" ]] && COLOR="%{$fg[white]%}%{$bg[red]%}"   # big bold label on root
  
  echo $COLOR
}

function reset_color {
  echo "%{$reset_color%}"
}

PROMPT="%n@%m:%~"                       # user@host:~
PROMPT="<%*> $PROMPT"                   # <hh:mm:ss>
PROMPT="\$(prompt_color)$PROMPT\$(reset_color) \$(git_super_status)$ " # git info
# TODO - bell if this term's owner isn't on current screen.  long running procs will always bell when done.  xprop could get ugly (xterm -> tmux -> vim), but maybe setting a global var on init would be cleaner?


ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[cyan]%}"
# maybe add functions escaped and then sed them up into $()

# Show pretty background jobs list unless empty
RPROMPT='%(1j.`jobs | sed -e "s/ .*  //" | tr -d "\n" | tr "]" ":" | tr "[" " "`.)' 
#RPROMPT="%F{54}$RPROMPT%f"

## Source some configs (.local files don't go in git)
if [[ "$-" == *i* ]] ; then  # only for interactive shells
   for FILE in .{alias,functions,zshrc,shellrc}{,.local,.$HOST} ; do
    [[ -e "$HOME/$FILE" && "$FILE" != ".zshrc" ]] && source "$HOME/$FILE"
  done
fi

true
