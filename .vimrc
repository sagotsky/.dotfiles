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
" !$PPID -> xwininfo -> font size
set clipboard+=unnamed            " yank -> X11 buffer
set laststatus=2                  " 2 lines for status
set lazyredraw
set ttyfast
set history=1000
set t_ZH=[3m                    " enable italics in some terms
set t_ZR=[23m                   " http://askubuntu.com/questions/492592/can-i-get-italics-in-gnome-terminal
" hi Comment cterm=bold       " urxvt supports multiple font faces.  xterm
" will stretch, but no good on source pro semi.  if urxvt can do live font
" size changes, let's switch

let g:ruby_doc_ruby_host='http://apidock.com/ruby/'
let mapleader=" "

if v:version >= 704
  set regexpengine=1                " older engine is somehow faster for ruby syntax highlighting
endif

" NeoBundle
if has('vim_starting')
  set nocompatible    
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundle 'Shougo/vimproc.vim', '0f68bcd93399ecbcde3eaa4efd09107314c9bdee', { 'build' : { 'linux' : 'make -f make_unix.mak ; cp -R autoload/* ~/.vim/autoload/; cp -R plugin/* ~/.vim/plugin', }, }

NeoBundleFetch 'Shougo/neobundle.vim'
"if v:version >= 704
  "NeoBundle 'Shougo/neocomplete'
"endif
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-rsi'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tomasr/molokai'
NeoBundle 'jpo/vim-railscasts-theme', '9035daff38dbbd30229890f26e54d9a7a71deca3'
NeoBundle 'godlygeek/tabular'
NeoBundle 'bling/vim-airline'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'kshenoy/vim-signature'                         " show marks in gutter
NeoBundle 'lilydjwg/colorizer'
NeoBundle 'joker1007/vim-ruby-heredoc-syntax'

if isdirectory($HOME."/.rbenv")
  NeoBundle 'vim-scripts/ruby-matchit'
  NeoBundle 'haml/haml-contrib'
  NeoBundle 'mustache/vim-mustache-handlebars'
  NeoBundle 'tpope/vim-rails'
  NeoBundle 'lucapette/vim-ruby-doc'
endif

" experimental
NeoBundle 'junegunn/vim-peekaboo'                         " preview yank ring
NeoBundle 'vim-scripts/SQLComplete.vim'
NeoBundle 'tpope/vim-commentary'                          " gcc -> comment.  #gc -> comment n lines
NeoBundle 'ludovicchabant/vim-gutentags'                  " auto generate tags.  is it doing rtags?
NeoBundle 'benmills/vimux'
NeoBundle 'jgdavey/vim-turbux'
NeoBundle 'kien/ctrlp.vim'
let g:turbux_runner = 'vimux'

NeoBundleCheck
call neobundle#end()

let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='jellybeans'
let g:airline#extensions#hunks#enabled=0
let g:airline#extensions#branch#enabled=0

let g:syntastic_auto_loc_list=1
let g:syntastic_quiet_messages = {'level': 'warnings'}
let g:syntastic_coffee_coffeelint_args = "--file ".$HOME."/.coffeelint.json"
"let g:syntastic_filetype_map = { "mustache": "handlebars" }

" Unite.vim options and keys
let g:unite_source_rec_max_cache_files = 0
let g:unite_split_rule = 'botright' 
call unite#custom#source('grep', 'max_candidates', 0)
call unite#custom#source('file_rec,file_rec/async,grepocate', 'max_candidates', 0)
call unite#custom#source('file_rec,file_rec/async,file_mru,file,buffer,grep',
  \ 'ignore_pattern', join([
    \ '\.git/',
    \ 'public/source_maps/',
    \ 'frontend/\(tmp\|bower_components\|node_modules\|public\|vendor\)/',
    \ '^tmp/',
    \ '^shared_js/',
    \ '^survey_creation_app/',
    \ '^/log/',
    \], '\|'))
let g:unite_source_mark_marks = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "0123456789.'`^<>[]{}()\"
" create a custom find that pipes through sort?  now app shows up first.

" unite customizations TODO
" - find last n files
" - find files edited in branch.  comparing to master doesn't work when master
"   has since gotten merged commits
" - filter out unwanted folders.  works in ag, but no options for find?

let g:unite_enable_start_insert=1
nnoremap <leader>p          :Unite file_rec/async <cr>
nnoremap <leader>h          :Unite outline<cr>
nnoremap <leader>b          :Unite buffer <cr>
nnoremap <leader>m          :Unite mark<cr>
nnoremap <leader>r          :Unite file_mru<cr>
nnoremap <leader>R          :Unite directory_mru<cr>
nnoremap <leader>/          :Unite -no-start-insert grep:. <cr>
nnoremap <leader>?          :UniteWithCursorWord -no-start-insert grep:. <cr>
nnoremap <leader>ur         :UniteResume -no-start-insert<cr>

nnoremap <leader>wh         :vertical resize -10<cr>
nnoremap <leader>wl         :vertical resize +10<cr>
nnoremap <leader>wj         :resize +10<cr>
nnoremap <leader>wk         :resize -10<cr>

nnoremap <leader>gs         :Gstatus<cr>
nnoremap <leader>gc         :Gcommit<cr>
nnoremap <leader>gb         :! xterm -e 'zsh -i -c "git cb"'<cr>
nnoremap <leader>gB         :Gblame<cr>
nnoremap <leader>gh         :Gbrowse<cr>
nnoremap <leader>gp         :Git pull<cr>
nnoremap <leader>gP         :Git pp<cr>
" branch, checkout are taken.  what else works here?  f for feature?
nnoremap <leader>rc         :! run-in-term.sh rails c <cr><cr>
nnoremap <leader>rd         :! run-in-term.sh rails db <cr><cr>

nnoremap <Leader>z          :Zeal <cr>


imap jj <Esc>
map <F7> :set invspell<CR>
map <F6> :set invwrap<CR>
map <F8> :set invnumber<CR>
map <leader><S-CR> :! xterm &<cr><cr>
map <leader>s :e ~/.vimrc<cr>
map <leader>S :so ~/.vimrc<cr>


let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'


if executable('ag')
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --nogroup -S --ignore flex --ignore tmp --ignore "*source_maps*" --ignore "*.log"' " ignore doens't take a regex.  ^public/.* eludes me
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
:com! Bd bd 
:com! Bdall 0,9999 bd
:com! Ebranch args `git ls-branch`

" accidental quit prevention (use :quit instead) http://stackoverflow.com/questions/12556267/how-to-prevent-quitting-vim-accidentally
"cabbrev q <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'echo "Denied!  Try :Q or :quit to quit the last window."<bar>close' : 'q')<cr>

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

function! Zeal()
  let word = expand("<cword>")
  execute 'silent !killall -9 zeal '
  execute 'silent !zeal -f -q ' . word . '&>/dev/null &'
endfunction
command! Zeal call Zeal()
" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)
" (could something map to shift-insert??)
"
" N normal, I insert, V visual+select, S select, X visual, C command, O oper
"source ~/.vim/readline.vim

filetype plugin on		"enable filetype plugin
filetype indent on

set fillchars+=vert:│

if ( $TERM != 'linux')          "don't break vim in vterms
  set t_Co=256                "ensures 256 color
  colorscheme railscasts 
  execute "silent !TERM=xterm xtermcontrol --bg 'rgb:20/20/20'"
  "highlight linenr 		ctermfg=darkgray	
  "highlight CursorLine 	ctermbg=235 cterm=bold 
  "    highlight String 		ctermfg=green 
  "    highlight Constant 		ctermfg=red 
  "highlight Comment 		ctermfg=darkgray
  highlight Search 		ctermfg=white ctermbg=237 cterm=none
  "    highlight Todo		ctermfg=21 ctermbg=11
  "highlight StatusLine	cterm=bold ctermfg=white ctermbg=black
  "highlight StatusLineNC	cterm=bold ctermfg=darkgray ctermbg=black
  highlight VertSplit		ctermfg=234 ctermbg=235
  "highlight SpellBad          ctermfg=229 cterm=underline
  "hi normal ctermbg=black
  hi Visual      ctermbg=236
  hi Pmenu                     ctermfg=gray ctermbg=235 gui=NONE
  hi PmenuSel                  ctermfg=white ctermbg=236 gui=NONE
  hi Comment      cterm=italic gui=italic

  " gitgutter
  hi SignColumn guibg=#202020 ctermbg=234
  hi GitGutterAddDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeDeleteDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeLineDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeDeleteDefault guibg=#202020 ctermbg=234
  hi GitGutterDeleteDefault guibg=#202020 ctermbg=234
endif

" indentguides colors
let g:indent_guides_auto_colors = 0
let g:indent_guides_enable_on_vim_startup = 0
hi IndentGuidesOdd  guibg=#202020   ctermbg=235
hi IndentGuidesEven guibg=#2a2a2a ctermbg=234

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
  "IndentGuidesEnable
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
