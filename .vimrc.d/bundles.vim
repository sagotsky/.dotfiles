if has('vim_starting')
  set nocompatible    
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-endwise'                 " ruby append end to new blocks
"Plug 'tpope/vim-repeat'
Plug 'tpope/vim-fugitive'                " git integration
Plug 'tpope/vim-surround'                " change surrounding characters with cs prefix
Plug 'tpope/vim-rsi'                     " readline in insert mode
Plug 'nathanaelkane/vim-indent-guides'   " stripes indentation
Plug 'airblade/vim-gitgutter'            " duh
Plug 'jgdavey/vim-railscasts'            " theme
Plug 'godlygeek/tabular'                 " Aligns by regex
Plug 'vim-airline/vim-airline'           " bar
Plug 'vim-airline/vim-airline-themes'    " bar themes
Plug 'scrooloose/syntastic'              " syntax/linter
Plug 'christoomey/vim-tmux-navigator'    " buffer nav bindings
Plug 'kshenoy/vim-signature'             " show marks in gutter
Plug 'ap/vim-css-color'                  " shows hex colors in color
Plug 'mhinz/vim-grepper'                 " replaces ag.vim.  uses neovim dispatch.
Plug 'ervandew/supertab'                 " tab for autocomplete

if isdirectory($HOME."/.rbenv")
  Plug 'vim-scripts/ruby-matchit'          " % support for do/end
  Plug 'haml/haml-contrib'                 " haml syntax highlighting
  Plug 'joker1007/vim-ruby-heredoc-syntax' " syntax highlight heredocs in ruby
  Plug 'sagotsky/vim-turbux'               " tmux -> rails testing
  Plug 'benmills/vimux'                    " tmux -> rails testing
  Plug 'mtscout6/vim-cjsx'                 " cjsx highlighting
  Plug 'vim-ruby/vim-ruby'                 " ruby specific shortcuts
  "Plug 'mustache/vim-mustache-handlebars'
  "Plug 'tpope/vim-rails'
endif

" experimental
Plug 'junegunn/vim-peekaboo'        " preview yank ring
Plug 'vim-scripts/SQLComplete.vim'  " SQL syntax highlighting
Plug 'tpope/vim-commentary'         " gcc -> comment.  #gc -> comment n lines
Plug 'ludovicchabant/vim-gutentags' " auto generate tags.  is it doing rtags?
Plug 'ctrlpvim/ctrlp.vim'           " file picker
Plug 'wincent/terminus'             " more term support.  mouse?
Plug 'pbrisbin/vim-mkdir'           " mkdir support
Plug 'scrooloose/nerdtree'          " file selection
Plug 'vim-scripts/sql_iabbr.vim'    " sql commands are capitalized
Plug 'junegunn/gv.vim'              " git log viewer.  :GV
Plug 'vimwiki/vimwiki'              " internal wiki

call plug#end()
