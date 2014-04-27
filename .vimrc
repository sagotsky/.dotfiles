syntax on 				"always use syntax highlighting
set nowrap 				"no word wrap
set smartcase 			"case affects word boundaries
set autoindent 			"emacs style indents ==========broken
set smartindent  		"emacs style indents ==========broken
set tabstop=2 			"tab width
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
set showcmd             " keybindings show their full name
set guioptions=aegiLt     "clean gui in gvim
set guifont=Source\ Code\ Pro\ 10   " Droid\ Sans\ Mono\ 10 
set clipboard+=unnamed    " yank -> X11 buffer
let mapleader=" "


" NeoBundle
if has('vim_starting')
  set nocompatible    
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
call neobundle#rc(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neocomplete'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc.vim'
NeoBundle 'h1mesuke/unite-outline'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'lucapette/vim-ruby-doc'

NeoBundleCheck


" Unite.vim options and keys
let g:unite_source_rec_max_cache_files = 0
let g:unite_split_rule = 'botright' 
call unite#custom#source('grep', 'max_candidates', 0)
call unite#custom#source('file_rec,file_rec/async,grepocate', 'max_candidates', 0)
" C-l in a unite to refresh.  https://github.com/Shougo/unite.vim/issues/374

let g:unite_enable_start_insert=1
nnoremap <leader>p          :Unite file_rec/async <cr>
nnoremap <leader>h          :Unite outline<cr>
nnoremap <leader>b          :Unite buffer<cr>
nnoremap <leader>t          :Unite tag<cr>
nnoremap <leader>T          :UniteWithCursorWord tag<cr>

nnoremap <leader>g          :Unite -no-start-insert grep:. <cr>
nnoremap <leader>G          :UniteWithCursorWord -no-start-insert grep:. <cr>

if executable('ag')
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --nogroup -S --ignore flex --ignore tmp'
  let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts='--no-heading --no-color -a '
  let g:unite_source_grep_recursive_opt=''
endif


try                             "persistent undo files
  set undodir=~/.vim_runtime/undodir
  set undofile
catch
endtry

:com! Sudow !sudo tee %	
:com! W w
:com! Q q

map <F7> :set invspell<CR>
map <F6> :set invwrap<CR>
map <F8> :set invnumber<CR>
map <leader><S-CR> :! xterm &<cr><cr>
map <leader>s :e ~/.vimrc<cr>
map <leader>S :so ~/.vimrc<cr>

" accidental quit prevention (use :quit instead) http://stackoverflow.com/questions/12556267/how-to-prevent-quitting-vim-accidentally
cabbrev q <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'echo "Denied!  Try :Q or :quit to quit the last window."<bar>close' : 'q')<cr>

let s:pattern = '^\(.* \)\([1-9][0-9]*\)$'
let s:minfontsize = 6
let s:maxfontsize = 16
function! AdjustFontSize(amount)
  if has("gui_gtk2") && has("gui_running")
    let fontname = substitute(&guifont, s:pattern, '\1', '')
    let cursize = substitute(&guifont, s:pattern, '\2', '')
    let newsize = cursize + a:amount
    if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
      let newfont = fontname . newsize
      let &guifont = newfont
      " try launching xterm -e exit or something
      silent !xterm -e sleep .1;exit
    endif
  else
    echoerr "You need to run the GTK2 version of Vim to use this function."
  endif
endfunction

function! LargerFont()
  call AdjustFontSize(1)
endfunction
command! LargerFont call LargerFont()

function! SmallerFont()
  call AdjustFontSize(-1)
endfunction
command! SmallerFont call SmallerFont()

map <leader>+ :LargerFont<CR>
map <leader>- :SmallerFont<CR>

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


  " gitgutter
  hi SignColumn guibg=#202020 ctermbg=233
  hi GitGutterAddDefault guibg=#202020 ctermbg=233
  hi GitGutterChangeDefault guibg=#202020 ctermbg=233
  hi GitGutterChangeDeleteDefault guibg=#202020 ctermbg=233
  hi GitGutterChangeLineDefault guibg=#202020 ctermbg=233
  hi GitGutterChangeDeleteDefault guibg=#202020 ctermbg=233
  hi GitGutterDeleteDefault guibg=#202020 ctermbg=233
endif

" indentguides colors
let g:indent_guides_auto_colors = 0
let g:indent_guides_enable_on_vim_startup = 0
hi IndentGuidesOdd  guibg=#202020   ctermbg=3
hi IndentGuidesEven guibg=#2a2a2a ctermbg=4

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
au BufNewFile,BufRead .vimrc set filetype=vim

au BufRead,BufNewFile *.js set filetype=javascript
au BufRead,BufNewFile *.json set filetype=javascript

au BufRead,BufNewFile *.xmobarrc set filetype=haskell
au BufRead,BufNewFile *.hs set filetype=haskell

au! BufRead,BufNewFile *.haml         call HamlSettings()
function! HamlSettings()
  setfiletype haml 
  IndentGuidesEnable
endfunction

au BufRead,BufNewFile *.rb        call RubySettings()
function! RubySettings()
  ab pry binding.pry
endfunction

au BufRead,BufNewFile COMMIT_EDITMSG     set textwidth=0 
au BufRead,BufNewFile COMMIT_EDITMSG     set wrap
au BufRead,BufNewFile COMMIT_EDITMSG     set spell
