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
Plug 'NLKNguyen/papercolor-theme'      " light mode theme
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

Plug 'itchyny/lightline.vim'
let g:lightline = {
  \ 'colorscheme': 'jellybeans',
  \ 'active': {
  \ 'right': [ ['lineinfo'],
  \            ['percent'] ]
  \ },
  \ 'inactive': {
  \   'right': []
  \ },
  \ 'component_function': {
  \   'filename': 'LightLineFilename'
  \ }
\}
function! LightLineFilename()
  return expand('%')
endfunction

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
  let g:turbux_test_type = 'rspec' " https://github.com/jgdavey/vim-turbux/blob/master/plugin/turbux.vim
  " let g:turbux_command_test_unit = 'bin/rails test'     " default: ruby -Itest
  let g:turbux_command_test_unit = 'bundle exec ruby -Itest '     " default: ruby -Itest
  let g:turbux_command_rspec = 'bundle-or-docker bundle exec rspec'

  " TODO: play with runner that puts file links in quickfix
  "
  " NO_CLI fixes docker.  still no quickfix.  wrong compiler?
  " Plug 'tpope/vim-dispatch'
  " Plug 'janko/vim-test'
  " https://github.com/docker/compose/issues/5696 workaround for the not a TTY problem
  " let test#ruby#rspec#executable = 'COMPOSE_INTERACTIVE_NO_CLI=1 docker/run bundle exec rspec'
  " map <Leader>j TestNearest<CR>

  " Plug 'radenling/vim-dispatch-neovim'
  " Plug 'neomake/neomake'
  " same problem with docker
  " Plug 'thoughtbot/vim-rspec'
  " let g:rspec_command = "!COMPOSE_INTERACTIVE_NO_CLI=1 docker/run bundle exec rspec %"
  " map <Leader>t :call RunCurrentSpecFile()<CR>
  " temporary key for playing with this plugin.
  " map <Leader>j :call RunCurrentSpecFile()<CR>

  Plug 'vim-ruby/vim-ruby'                 " ruby specific shortcuts
  Plug 'tpope/vim-rails'

  Plug 'joker1007/vim-ruby-heredoc-syntax' " syntax highlight heredocs in ruby
  " Add syntax rule
  let g:ruby_heredoc_syntax_filetypes = {
          \ "graphql" : {
          \   "start" : "GRAPHQL",
          \},
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
let g:ale_fix_on_save = 1
let g:ale_lint_on_enter = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_set_highlights = 1
let g:ale_sign_error = "⇨"
let g:ale_sign_warning = "•"
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_ruby_rubocop_executable = 'rubocop-vim.sh'
" let g:ale_ruby_rubocop_executable = 'bin/rubocop'
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

" requires nodejs, yarn
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
Plug 'neoclide/coc-solargraph', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-css', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-json', {'do': 'yarn install --frozen-lockfile'}

Plug 'sheerun/vim-polyglot'
Plug 'jparise/vim-graphql'
Plug 'enricobacis/paste.vim' " fast paste
Plug 'yuttie/comfortable-motion.vim' " smooth scrolling so pair partners don't get lost
let g:comfortable_motion_friction = 80.0
let g:comfortable_motion_air_drag = 2.0

Plug 'powerman/vim-plugin-AnsiEsc' " parse ansi colors for logs
Plug 'inside/vim-search-pulse' " flashy search results
let g:vim_search_pulse_duration = 150
let g:vim_search_pulse_mode = 'pattern'

Plug 'rhysd/git-messenger.vim'
let g:git_messenger_include_diff = 1

call plug#end()
