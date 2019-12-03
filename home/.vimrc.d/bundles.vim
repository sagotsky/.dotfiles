if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')


Plug 'mhinz/vim-signify' " faster than gitgutter?
let g:signify_sign_change = '~' "not really a color...
let g:signify_sign_show_count = 'false'

" Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
" let g:Hexokinase_highlighters = ["foregroundfull", "virtual", "sign_column", "backgroundfull"]
" Plug 'https://github.com/gko/vim-coloresque' " override FT to make it work

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

  Plug 'jgdavey/vim-turbux'               " tmux -> rails testing
  Plug 'benmills/vimux'                    " tmux -> rails testing
  let g:VimuxHeight = "30"
  let g:turbux_command_rspec = 'docker/run-p rspec'

  Plug 'vim-ruby/vim-ruby'                 " ruby specific shortcuts
  Plug 'tpope/vim-rails'

  Plug 'joker1007/vim-ruby-heredoc-syntax' " syntax highlight heredocs in ruby
  " Add syntax rule
  let g:ruby_heredoc_syntax_filetypes = {
          \ "graphql" : {
          \   "start" : "GRAPHQL",
          \},
          \ "yaml" : {
          \   "start" : "YML",
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
let g:fzf_files_options =
  \ '--preview "bat {} --color=always --style=numbers,changes 2> /dev/null | head -'.&lines.'"'
command! -bang -nargs=* Rg
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%', '?'),
  \                 <bang>0)
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%', '?'),
  \                 <bang>0)

" trying vim-coc instead
" ale does better auto fix by far
Plug 'w0rp/ale'
let g:ale_fix_on_save = 1
let g:ale_lint_on_enter = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_set_highlights = 1
let g:ale_sign_error = "✘"
let g:ale_sign_warning = "•"
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_ruby_rubocop_executable = 'rubocop-vim.sh'
" let g:ale_ruby_rubocop_executable = 'bin/rubocop'
let g:ale_linters = {
\ 'ruby': ['ruby', 'rubocop', 'solargraph'],
\ 'haml': ['haml-lint']
\}
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['eslint'],
\   'ruby': ['rubocop'],
\}

" requires nodejs, yarn
Plug 'neoclide/coc.nvim', {'brand': 'release', 'do': { -> coc#util#install()}}

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

Plug 'andrewradev/splitjoin.vim' " gS and gJ for spliting and joining lines
Plug 'andrewradev/switch.vim' " gs switches syntax.  good for hash.  disagree with some of the options...

Plug 'junegunn/goyo.vim' " writing mode.  :Goyo
function! s:goyo_enter()
  set wrap linebreak
  set spell
  nnoremap j gj
  nnoremap k gk
endfunction

function! s:goyo_leave()
  set nowrap
  set nospell
  unmap j
  unmap k
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

Plug 'troydm/zoomwintab.vim' " <C-w>o zoom a pane

call plug#end()
