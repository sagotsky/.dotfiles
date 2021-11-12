set runtimepath+=/usr/share/nvim/runtime
syntax on                         " always use syntax highlighting

set clipboard+=unnamed            " yank -> X11 buffer
set cursorline                    " underlines current line
set expandtab                     " inserts spaces instead of tabs
set fillchars+=vert:│
set guifont=Source\ Code\ Pro\ 12 " gvim only font
set guioptions=aegiLt             " clean gui in gvim
set hlsearch                      " highlights search results
set inccommand=nosplit            " live preview of substitutions
set ignorecase                    " case insensitive searches with /
" set lazyredraw # with this on, resizes blanked screen
set nofoldenable                  " disable folds
set nowrap                        " no word wrap
set number                        " line numbers on
set scrolloff=4                   " scroll at 4 lines from top/bottom
set shiftwidth=2                  " tab width
set showcmd                       " keybindings show their full name
set showmatch                     " shows matching parens
set smartcase                     " case affects word boundaries
set smartindent                   " emacs style indents ==========broken
set softtabstop=2                 " deletes 4 spaces as though they were a tab.
set t_ZH=[3m                    " enable italics in some terms
set t_ZR=[23m                   " http://askubuntu.com/questions/492592/can-i-get-italics-in-gnome-terminal
set tabstop=2                     " tab width
set ttyfast
set wildmode=longest,list,full    " tab completion fix.  completes as much as possible, then lists, then full completes.
set grepprg=rg\ --vimgrep         " use ripgrep for search


" set runtimepath+=~/.vim,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after

" if v:version >= 704
"   set regexpengine=1                " older engine is somehow faster for ruby syntax highlighting
" endif

try                             "persistent undo files
  set undodir=~/.vim_runtime/undodir
  set undofile
catch
endtry

" Source all the .vim files in ~/.vimrc.d, starting with bundles
source $HOME/.vimrc.d/bundles.vim
for file in split(glob('~/.vimrc.d/*.vim'), '\n')
  if split(file, '/')[-1] != 'bundles.vim'
    exe 'source' file
  endif
endfor

filetype indent on

" todo: hide the autocmds in a file
autocmd VimResized * exe "normal \<c-w>="

" install new bundles after saving bundles file
if !exists("*BundlesUpdated")
  function BundlesUpdated()
    so ~/.dotfiles/.vimrc.d/bundles.vim
    PlugInstall
  endfunction
endif

autocmd BufWritePre * %s/\s\+$//e
