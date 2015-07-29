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
let colorcolumn = join(range(121,999), ",")

if v:version >= 704
  set regexpengine=1                " older engine is somehow faster for ruby syntax highlighting
endif

try                             "persistent undo files
  set undodir=~/.vim_runtime/undodir
  set undofile
catch
endtry

" Source all the .vim files in ~/.vimrc.d
for file in split(glob('~/.vimrc.d/*.vim'), '\n')
  exe 'source' file
endfor

"executes the command on quit
" is this worth keeping?  why does it exist?
 augroup autocom
     autocmd!
     autocmd VimLeave * !TERM=xterm xtermcontrol --bg rgb:0000/0000/0000
 augroup END

filetype plugin indent on		
