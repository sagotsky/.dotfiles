syntax on 				"always use syntax highlighting
set nowrap 				"no word wrap
set smartcase 			"case affects word boundaries
set autoindent 			"emacs style indents ==========broken
set smartindent  		"emacs style indents ==========broken
set tabstop=4 			"tab width
set shiftwidth=2 		"tab width
set softtabstop=2		"deletes 4 spaces as though they were a tab.
set showmatch 			"shows matching parens
set ruler 				"line/position always visible
set incsearch 			"search as you type
set cursorline 			"underlines current line
set number				"line numbers on
set ignorecase			"case insensitive searches with /
set hlsearch			"highlights search results                             
set scrolloff=4         "scroll at 4 lines from top/bottom
set expandtab			"inserts spaces instead of tabs
set wildmode=longest,list,full    "tab completion fix.  completes as much as possible, then lists, then full completes.

set guioptions=aegiLt     "clean gui in gvim

let Tlist_WinWidth = 50   "taglist width

try                             "persistent undo files
    set undodir=~/.vim_runtime/undodir
    set undofile
catch
endtry

:com Sudow !sudo tee %	
:com W w
:com Q q

map <F7> :set invspell<CR>
map <F6> :set invwrap<CR>
map <F8> :set invnumber<CR>
map <F9> :TlistToggle<CR>

" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)
" (could something map to shift-insert??)
"
" N normal, I insert, V visual+select, S select, X visual, C command, O oper
source ~/.vim/readline.vim

filetype plugin on		"enable filetype plugin
filetype indent on

colorscheme ambient 
if ( $TERM != 'linux')          "don't break vim in vterms
    set t_Co=256                "ensures 256 color
    highlight linenr 		ctermfg=darkgray	
    highlight CursorLine 	ctermbg=235 cterm=bold 
"    highlight String 		ctermfg=green 
"    highlight Constant 		ctermfg=red 
    highlight Comment 		ctermfg=darkgray
    highlight Search 		ctermfg=white ctermbg=33
"    highlight Todo		ctermfg=21 ctermbg=11
    highlight StatusLine	cterm=bold ctermfg=white ctermbg=black
    highlight StatusLineNC	cterm=bold ctermfg=darkgray ctermbg=black
    highlight VertSplit		ctermfg=black ctermbg=black
    highlight SpellBad          ctermfg=229 cterm=underline
endif
"
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
au BufNewFile,BufRead .vimrc set filetype=vim

au BufRead,BufNewFile *.js set filetype=javascript
au BufRead,BufNewFile *.json set filetype=javascript

au BufRead,BufNewFile *.xmobarrc set filetype=haskell
au BufRead,BufNewFile *.hs set filetype=haskell

au! BufRead,BufNewFile *.haml         setfiletype haml 

au BufRead,BufNewFile COMMIT_EDITMSG     set textwidth=0 
au BufRead,BufNewFile COMMIT_EDITMSG     set wrap
au BufRead,BufNewFile COMMIT_EDITMSG     set spell
