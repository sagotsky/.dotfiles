au BufRead,BufNewFile *.txt          set filetype=text

au BufRead,BufNewFile *.4e.txt       set filetype=4e

au BufNewFile,BufRead *.rc           set filetype=sh
au BufNewFile,BufRead .xsession      set filetype=sh
au BufNewFile,BufRead .functions     set filetype=sh
au BufNewFile,BufRead .alias         set filetype=sh
au BufNewFile,BufRead *.conf         set filetype=sh

au BufRead,BufNewFile *.md           set filetype=mkd

au BufRead,BufNewFile *.install      set filetype=php
au BufRead,BufNewFile *.drush        set filetype=php
au BufRead,BufNewFile *.profile      set filetype=php
au BufRead,BufNewFile *.test         set filetype=php
au BufRead,BufNewFile *.module       set filetype=php
au BufRead,BufNewFile *.inc          set filetype=php
au BufRead,BufNewFile *.php          set filetype=php

au BufRead,BufNewFile *.py           set filetype=python

au BufNewFile,BufRead .pentadactylrc set filetype=vim
au BufNewFile,BufRead .vimperatorrc  set filetype=vim
au BufNewFile,BufRead .vimrc         set filetype=vim

au BufRead,BufNewFile *.js           set filetype=javascript
au BufRead,BufNewFile *.json         set filetype=javascript
au BufRead,BufNewFile *.coffee       set filetype=javascript
au BufRead,BufNewFile *.coffee.erb   set filetype=javascript

au BufRead,BufNewFile *.xmobarrc     set filetype=haskell
au BufRead,BufNewFile *.hs           set filetype=haskell
