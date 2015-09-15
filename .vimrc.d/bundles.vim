if has('vim_starting')
  set nocompatible    
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundle 'Shougo/vimproc.vim', '0f68bcd93399ecbcde3eaa4efd09107314c9bdee', { 'build' : { 'linux' : 'make -f make_unix.mak ; cp -R autoload/* ~/.vim/autoload/; cp -R plugin/* ~/.vim/plugin', }, }

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-rsi'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tomasr/molokai'
NeoBundle 'jpo/vim-railscasts-theme', '9035daff38dbbd30229890f26e54d9a7a71deca3'
NeoBundle 'Wutzara/vim-materialtheme'
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
NeoBundle 'jgdavey/vim-turbux'
"NeoBundle 'tpope/vim-dispatch'
NeoBundle 'benmills/vimux'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'tacahiroy/ctrlp-funky'
NeoBundle 'mtscout6/vim-cjsx'
NeoBundle 'wincent/terminus'
NeoBundle 'rking/ag.vim'
NeoBundle 'scrooloose/vimfiles'
NeoBundle 'pbrisbin/vim-mkdir'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'ryanoasis/vim-devicons'
NeoBundle 'ervandew/supertab'
"NeoBundle 'malept/vim-flog' why you no work


NeoBundleCheck
call neobundle#end()
