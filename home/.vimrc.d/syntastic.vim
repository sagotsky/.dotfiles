let g:syntastic_auto_loc_list=0
let g:syntastic_check_on_open=1
let g:syntastic_coffee_coffeelint_args = "--file ".$HOME."/.coffeelint.json"
let g:syntastic_cjsx_checkers=[]
let g:syntastic_haml_checkers = ['haml_lint'] ", 'rubocop']
let g:syntastic_ruby_checkers = ['mri', 'flog'] ", 'rubocop']
let g:syntastic_ruby_rubocop_exec = $HOME . "/bin/rubocop"
let g:syntastic_ruby_flog_args = '--details'
let g:syntastic_disabled_filetypes = ['sql']
let g:syntastic_always_populate_loc_list=1 " lprev/lnext work without list now

let g:syntastic_ruby_flog_threshold_warning=20
let g:syntastic_ruby_flog_threshold_error=40

let g:syntastic_quiet_messages = {
  \ 'regex': '\(interpreted as argument\|Prefer single-quoted\|Space missing inside\|Space inside . missing\|too many lines\|too long\|Missing top-level class documentation\|Use nested module\|literals should be delimited\|Do not use block comments\|Extra empty line\)'}


let g:syntastic_error_symbol = "✖"
let g:syntastic_warning_symbol = "▶"
let g:syntastic_style_error_symbol = "•"
let g:syntastic_style_warning_symbol = "•"

" stashing ale config here so these can stay consistent
let g:ale_sign_error = "•"
let g:ale_sign_warning = "•"
hi ALEWarningSign ctermfg=240
hi ALEErrorSign ctermfg=202

let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
 " rubucop, reek, rails_best_practices
let g:ale_linters = {
\ 'ruby': ['ruby', 'rubocop'],
\ 'haml': ['haml-lint']
\}
let g:ale_fixers = {
\   'javascript': ['eslint'],
\   'ruby': ['rubocop'],
\}
let g:ale_fix_on_save = 0
let g:ale_lint_on_enter = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
" let g:ale_ruby_rubocop_options = '--config ~/.rubocop.yml'
let g:ale_set_highlights = 0
