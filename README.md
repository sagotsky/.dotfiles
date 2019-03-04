# Sagotsky's dotfiles and scripts.

I link to these things often enough that they might as well be on github.

## App specific

### zsh
* aliases https://github.com/sagotsky/.dotfiles/blob/master/home/.alias
* functions https://github.com/sagotsky/.dotfiles/blob/master/home/.functions
* rc https://github.com/sagotsky/.dotfiles/blob/master/home/.zshrc

### Vim
* vimrc https://github.com/sagotsky/.dotfiles/blob/master/home/.vimrc
* vimrc.d - split up by plugin https://github.com/sagotsky/.dotfiles/tree/master/home/.vimrc.d

### Rails (for dev machiens)
* shell aliases, readline shortcuts https://github.com/sagotsky/.dotfiles/blob/master/home/.zsh_rails
* pry https://github.com/sagotsky/.dotfiles/blob/master/home/.pryrc

### xmonad
* xmonad.hs https://github.com/sagotsky/.dotfiles/blob/master/home/.xmonad/xmonad.hs

## Scripts

Whole lot of scripts in here.  Some will eventually be extracted into their own
repos.  Here are some of the most interesting ones.

### feature-db.sh

Clones a rails db on demand from a hot spare.  I put this in my
.env.development.local as the DB name so I always get a new database per
feature branch

*  https://github.com/sagotsky/.dotfiles/blob/master/home/scripts/feature-db.sh

### cli-board (script snippets) and cheat sheet

Dmenu/rofi for traversing a fey key directories.  They contain scripts that output
directly to the clipboard (ie, screenshot -> imgur -> copy url ) or copy snippets from
text files.

* Key bindings: https://github.com/sagotsky/.dotfiles/blob/master/home/.xmonad/xmonad.hs#L207-L208
* Cheat sheet: https://github.com/sagotsky/.dotfiles/blob/master/home/scripts/cheat-sheet.sh
  * cheat sheets: https://github.com/sagotsky/.dotfiles/tree/master/home/.cheat-sheets
* cli-board: https://github.com/sagotsky/.dotfiles/blob/master/home/scripts/cli-board.sh
  * scripts: https://github.com/sagotsky/.dotfiles/tree/master/home/.cli-board
