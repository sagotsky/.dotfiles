if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')


Plug 'mhinz/vim-signify' " faster than gitgutter?
let g:signify_sign_change = '~' "not really a color...
let g:signify_sign_show_count = 'false'

" Plug 'ap/vim-css-color'                " shows hex colors in color
" Plug 'RRethy/vim-hexokinase'           " hex color displayer.   looks nice but doesn't hit all the color patterns yet.

Plug 'christoomey/vim-tmux-navigator'  " buffer nav bindings
Plug 'ervandew/supertab'               " tab for autocomplete
Plug 'godlygeek/tabular'               " Aligns by regex
Plug 'jgdavey/vim-railscasts'          " theme
Plug 'kshenoy/vim-signature'           " show marks in gutter

Plug 'ludovicchabant/vim-gutentags'    " auto generate tags.  is it doing rtags?
let g:gutentags_ctags_exclude = ['*/node_modules/*', 'tmp', 'logs']

Plug 'mhinz/vim-grepper'               " replaces ag.vim.  uses neovim dispatch.
Plug 'nathanaelkane/vim-indent-guides' " stripes indentation
let g:indent_guides_auto_colors = 0
let g:indent_guides_enable_on_vim_startup = 0

Plug 'tpope/vim-commentary'            " gcc -> comment.  #gc -> comment n lines
Plug 'tpope/vim-endwise'               " ruby append end to new blocks
Plug 'tpope/vim-fugitive'              " git integration
Plug 'tpope/vim-repeat'                " binds . for vim-surround
Plug 'tpope/vim-rsi'                   " readline in insert mode
Plug 'tpope/vim-rhubarb'               " readline in insert mode
Plug 'tpope/vim-sensible'              " common defaults
Plug 'tpope/vim-surround'              " change surrounding characters with cs prefix
Plug 'tpope/vim-vinegar'               " file selection
Plug 'tpope/vim-eunuch'                " wraps unix commands in vim.  trying out :Move for renaming file and buffer at once.
Plug 'tpope/vim-abolish'               " provides :S, which uses brace expansion to handle weird replacements like case or tense

Plug 'vim-airline/vim-airline'         " bar
Plug 'vim-airline/vim-airline-themes'  " bar themes
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='jellybeans'
let g:airline#extensions#hunks#enabled=0
let g:airline#extensions#branch#enabled=0
"let g:airline_section_a=''
"let g:airline_section_b=''
"let g:airline_section_c=''
let g:airline_section_x=''
let g:airline_section_y=''

"let g:airline_section_z='' # 100% 9: 37
" TODO: make inactive a little more obvious looking.  remove black bg?

Plug 'vim-scripts/SQLComplete.vim'     " SQL syntax highlighting
Plug 'vimwiki/vimwiki'                 " internal wiki
let g:vimwiki_list = [{'path':'~/.vimwiki', 'syntax':'markdown'}]

Plug 'wincent/terminus'                " more term support.  mouse?

if isdirectory($HOME."/.rbenv")
  Plug 'vim-scripts/ruby-matchit'          " % support for do/end
  Plug 'sagotsky/vim-turbux'               " tmux -> rails testing
  " Plug 'jgdavey/vim-turbux'               " tmux -> rails testing
  Plug 'benmills/vimux'                    " tmux -> rails testing
  let g:VimuxHeight = "30"
  let g:turbux_test_type = 'minitest' " https://github.com/jgdavey/vim-turbux/blob/master/plugin/turbux.vim
  " let g:turbux_command_test_unit = 'bin/rails test'     " default: ruby -Itest
  let g:turbux_command_test_unit = 'bundle exec ruby -Itest '     " default: ruby -Itest

  Plug 'vim-ruby/vim-ruby'                 " ruby specific shortcuts
  Plug 'tpope/vim-rails'

  Plug 'joker1007/vim-ruby-heredoc-syntax' " syntax highlight heredocs in ruby
  " Add syntax rule
  let g:ruby_heredoc_syntax_filetypes = {
          \ "haml" : {
          \   "start" : "HAML",
          \},
    \}
  " 'start' is heredoc start literal.
endif

" experimental
Plug 'pbrisbin/vim-mkdir'           " mkdir support
Plug 'haya14busa/incsearch.vim'

Plug 'bronson/vim-trailing-whitespace' " highlight trailing whitespace
Plug 'alcesleo/vim-uppercase-sql'

Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'kana/vim-textobj-user'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

Plug 'w0rp/ale'
let g:ale_fix_on_save = 0
let g:ale_lint_on_enter = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_set_highlights = 1
let g:ale_sign_error = "⇨"
let g:ale_sign_warning = "•"
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_linters = {
\ 'ruby': ['ruby', 'rubocop'],
\ 'shell': ['ruby', 'rubocop'],
\ 'haml': ['haml-lint']
\}
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['eslint'],
\   'ruby': ['rubocop'],
\}



Plug 'sheerun/vim-polyglot'
Plug 'enricobacis/paste.vim' " fast paste
Plug 'yuttie/comfortable-motion.vim' " smooth scrolling so pair partners don't get lost
let g:comfortable_motion_friction = 80.0
let g:comfortable_motion_air_drag = 2.0

Plug 'powerman/vim-plugin-AnsiEsc' " parse ansi colors for logs
Plug 'inside/vim-search-pulse' " flashy search results
let g:vim_search_pulse_duration = 150
let g:vim_search_pulse_mode = 'pattern'

call plug#end()

"let g:gutentags_ctags_exclude = ['build/']
