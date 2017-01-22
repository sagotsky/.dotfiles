#!/bin/bash

[[ "$#" == "1" ]] && cd "$1"
st="$(~/.zsh/plugins/zsh-git-prompt/gitstatus.py)"
[[ "$st" == "" ]] && exit

for var in branch remote staged conflicts changed untracked clean ; do
  POS=$(( $POS + 1 ))
  export $var=$(echo "$st" | head -n $POS | tail -n 1)
done

# colorize for xmobar?
(
  echo "$branch"
  [[ $remote != '0' ]]    && echo "$remote"
  [[ $clean != '1' ]]     && echo '|'
  [[ $staged != '0' ]]    && echo "•$staged"
  [[ $conflicts != '0' ]] && echo "x$conflicts"
  [[ $changed != '0' ]]   && echo "+$changed"
  [[ $untracked != '0' ]] && echo "…"
  [[ $clean == '1' ]]     && echo " " #"✓"
) | tr -d "\n"
