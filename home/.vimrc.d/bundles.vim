if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')

Plug 'airblade/vim-gitgutter'          " duh
Plug 'ap/vim-css-color'                " shows hex colors in color
Plug 'christoomey/vim-tmux-navigator'  " buffer nav bindings
Plug 'ctrlpvim/ctrlp.vim'              " file picker
Plug 'ervandew/supertab'               " tab for autocomplete
Plug 'godlygeek/tabular'               " Aligns by regex
Plug 'jgdavey/vim-railscasts'          " theme
Plug 'junegunn/gv.vim'                 " git log viewer.  :GV
Plug 'kshenoy/vim-signature'           " show marks in gutter
Plug 'ludovicchabant/vim-gutentags'    " auto generate tags.  is it doing rtags?
Plug 'mhinz/vim-grepper'               " replaces ag.vim.  uses neovim dispatch.
Plug 'nathanaelkane/vim-indent-guides' " stripes indentation
Plug 'scrooloose/syntastic'            " syntax/linter
Plug 'tpope/vim-commentary'            " gcc -> comment.  #gc -> comment n lines
Plug 'tpope/vim-endwise'               " ruby append end to new blocks
Plug 'tpope/vim-fugitive'              " git integration
Plug 'tpope/vim-repeat'                " binds . for vim-surround
Plug 'tpope/vim-rsi'                   " readline in insert mode
Plug 'tpope/vim-sensible'              " common defaults
Plug 'tpope/vim-surround'              " change surrounding characters with cs prefix
Plug 'tpope/vim-vinegar'               " file selection
Plug 'tpope/vim-eunuch'                " wraps unix commands in vim.  trying out :Move for renaming file and buffer at once.
Plug 'vim-airline/vim-airline'         " bar
Plug 'vim-airline/vim-airline-themes'  " bar themes
Plug 'vim-scripts/SQLComplete.vim'     " SQL syntax highlighting
Plug 'vim-scripts/sql_iabbr.vim'       " sql commands are capitalized
Plug 'vimwiki/vimwiki'                 " internal wiki
Plug 'wincent/terminus'                " more term support.  mouse?

if isdirectory($HOME."/.rbenv")
  Plug 'vim-scripts/ruby-matchit'          " % support for do/end
  Plug 'haml/haml-contrib'                 " haml syntax highlighting
  Plug 'joker1007/vim-ruby-heredoc-syntax' " syntax highlight heredocs in ruby
  Plug 'sagotsky/vim-turbux'               " tmux -> rails testing
  " Plug 'jgdavey/vim-turbux'               " tmux -> rails testing
  Plug 'benmills/vimux'                    " tmux -> rails testing
  Plug 'mtscout6/vim-cjsx'                 " cjsx highlighting
  Plug 'vim-ruby/vim-ruby'                 " ruby specific shortcuts
  "Plug 'mustache/vim-mustache-handlebars'
  "Plug 'tpope/vim-rails'
  Plug 'kchmck/vim-coffee-script'
endif

" experimental
Plug 'junegunn/vim-peekaboo'        " preview yank ring
Plug 'pbrisbin/vim-mkdir'           " mkdir support
Plug 'haya14busa/incsearch.vim'
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'Shougo/deoplete.nvim', { 'do' : 'vim +UpdateRemotePlugins +qall' }

" Plug 'osyo-manga/vim-over'  " preview search/replace.  doesn't work yet well enough yet 6/16
" Plug 'junegunn/vim-emoji' "  needs a differnt term or font
Plug 'bronson/vim-trailing-whitespace' " highlight trailing whitespace
Plug 'alcesleo/vim-uppercase-sql'

Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'kana/vim-textobj-user'
call plug#end()

