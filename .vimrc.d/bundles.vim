if has('vim_starting')
  set nocompatible    
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')

Plug 'Shougo/vimshell.vim'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-rsi'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'airblade/vim-gitgutter'
Plug 'tomasr/molokai'
Plug 'jgdavey/vim-railscasts' ", '9034daff38dbbd30229890f26e54d9a7a71deca3'
Plug 'Wutzara/vim-materialtheme'
Plug 'godlygeek/tabular'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/syntastic'
Plug 'christoomey/vim-tmux-navigator'
Plug 'kshenoy/vim-signature'                         " show marks in gutter
Plug 'ap/vim-css-color'
Plug 'joker1007/vim-ruby-heredoc-syntax'
Plug 'mhinz/vim-grepper'         " replaces ag.vim.  uses neovim dispatch.

if isdirectory($HOME."/.rbenv")
  Plug 'vim-scripts/ruby-matchit'
  Plug 'haml/haml-contrib'
  Plug 'mustache/vim-mustache-handlebars'
  Plug 'tpope/vim-rails'
  Plug 'lucapette/vim-ruby-doc'
endif

" experimental
Plug 'junegunn/vim-peekaboo'                         " preview yank ring
Plug 'vim-scripts/SQLComplete.vim'
Plug 'tpope/vim-commentary'                          " gcc -> comment.  #gc -> comment n lines
Plug 'ludovicchabant/vim-gutentags'                  " auto generate tags.  is it doing rtags?
" Plug 'jgdavey/vim-turbux'
Plug 'sagotsky/vim-turbux'
Plug 'benmills/vimux'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mtscout6/vim-cjsx'
Plug 'wincent/terminus'
Plug 'scrooloose/vimfiles'
Plug 'pbrisbin/vim-mkdir'
Plug 'vim-ruby/vim-ruby'
Plug 'scrooloose/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'ervandew/supertab'
Plug 'vim-scripts/sql_iabbr.vim'         
Plug 'junegunn/gv.vim'         " git log viewer.  :GV
Plug 'vimwiki/vimwiki'

call plug#end()
