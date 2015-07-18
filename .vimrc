syntax on                         " always use syntax highlighting
runtime macros/matchit.vim
set nowrap                        " no word wrap
set smartcase                     " case affects word boundaries
set autoindent                    " emacs style indents ==========broken
set smartindent                   " emacs style indents ==========broken
set tabstop=2                     " tab width
set shiftwidth=2                  " tab width
set softtabstop=2                 " deletes 4 spaces as though they were a tab.
set showmatch                     " shows matching parens
set ruler                         " line/position always visible
set incsearch                     " search as you type
set cursorline                    " underlines current line
set number                        " line numbers on
set ignorecase                    " case insensitive searches with /
set hlsearch                      " highlights search results
set scrolloff=4                   " scroll at 4 lines from top/bottom
set expandtab                     " inserts spaces instead of tabs
set wildmode=longest,list,full    " tab completion fix.  completes as much as possible, then lists, then full completes.
set showcmd                       " keybindings show their full name
set guioptions=aegiLt             " clean gui in gvim
set guifont=Source\ Code\ Pro\ 12 " gvim only font 
set clipboard+=unnamed            " yank -> X11 buffer
set laststatus=2                  " 2 lines for status
set lazyredraw
set ttyfast
set history=1000
set t_ZH=[3m                    " enable italics in some terms
set t_ZR=[23m                   " http://askubuntu.com/questions/492592/can-i-get-italics-in-gnome-terminal
set fillchars+=vert:│

if v:version >= 704
  set regexpengine=1                " older engine is somehow faster for ruby syntax highlighting
endif

try                             "persistent undo files
  set undodir=~/.vim_runtime/undodir
  set undofile
catch
endtry

for file in split(glob('~/.vimrc.d/*.vim'), '\n')
  exe 'source' file
endfor

filetype plugin on		"enable filetype plugin
filetype indent on

" Buffer types
au BufRead,BufNewFile *.txt set filetype=text
au BufRead,BufNewFile *.txt set wrap

" .4e syntax highlighter
au BufRead,BufNewFile *.4e.txt set filetype=4e
au! Syntax 4e source ~/.vim/syntax/4e.vim

au BufNewFile,BufRead *.rc set filetype=sh
au BufNewFile,BufRead .xsession set filetype=sh
au BufNewFile,BufRead .functions set filetype=sh
au BufNewFile,BufRead .alias set filetype=sh
au BufNewFile,BufRead *.conf set filetype=sh

au BufRead,BufNewFile *.md set filetype=mkd

au BufRead,BufNewFile *.install set filetype=php
au BufRead,BufNewFile *.drush set filetype=php
au BufRead,BufNewFile *.profile set filetype=php
au BufRead,BufNewFile *.test set filetype=php
au BufRead,BufNewFile *.module set filetype=php
au BufRead,BufNewFile *.inc set filetype=php
au BufRead,BufNewFile *.php set filetype=php

au BufRead,BufNewFile *.py set filetype=python

au BufNewFile,BufRead .pentadactylrc set filetype=vim
au BufNewFile,BufRead .vimperatorrc set filetype=vim
au BufNewFile,BufRead .vimrc set filetype=vim
au! BufRead, BufNewFile *.vim     call VimSettings()
function! VimSettings()
  au! Syntax vim source ~/.vim/syntax/vim-theme.vim
endfunction

au BufRead,BufNewFile *.js set filetype=javascript
au BufRead,BufNewFile *.json set filetype=javascript

au BufRead,BufNewFile *.xmobarrc set filetype=haskell
au BufRead,BufNewFile *.hs set filetype=haskell

au! BufRead,BufNewFile *.haml         call HamlSettings()
function! HamlSettings()
  setfiletype haml 
endfunction

au BufRead,BufNewFile *.rb        call RubySettings()
function! RubySettings()
  ab pry binding.pry
endfunction

au BufRead,BufNewFile *.js        call JavaScriptSettings()
au BufRead,BufNewFile *.coffee    call JavaScriptSettings()
function! JavaScriptSettings()
  ab c_log console.log
endfunction

au BufRead,BufNewFile COMMIT_EDITMSG     set textwidth=0 
au BufRead,BufNewFile COMMIT_EDITMSG     set wrap
au BufRead,BufNewFile COMMIT_EDITMSG     set spell

augroup autocom
    autocmd!
    "executes the command on quit
     autocmd VimLeave * !TERM=xterm xtermcontrol --bg rgb:0000/0000/000
augroup END
