# Sagotsky's dotfiles and scripts.

I link to these things often enough that they might as well be on github.

## zsh
* aliases https://github.com/sagotsky/.dotfiles/blob/master/home/.alias
* functions https://github.com/sagotsky/.dotfiles/blob/master/home/.functions
* rc https://github.com/sagotsky/.dotfiles/blob/master/home/.zshrc


## Vim
* vimrc https://github.com/sagotsky/.dotfiles/blob/master/home/.vimrc
* vimrc.d - split up by plugin https://github.com/sagotsky/.dotfiles/tree/master/home/.vimrc.d

## Rails (for dev machiens)
* shell aliases, readline shortcuts https://github.com/sagotsky/.dotfiles/blob/master/home/.zsh_rails
* pry https://github.com/sagotsky/.dotfiles/blob/master/home/.pryrc

## Scripts

Whole lot of scripts in here.  Some will eventually be extracted into their own
repos.  Here are some of the most interesting ones.

* https://github.com/sagotsky/.dotfiles/blob/master/home/scripts/feature-db.sh
Clones a rails db on demand from a hot spare.  I put this in my
.env.development.local as the DB name so I always get a new database per
feature branch

