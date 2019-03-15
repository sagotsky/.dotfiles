let g:ale_sign_error = "⇨"
let g:ale_sign_warning = "•"

" colors are in colors.vim
"
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
 " rubucop, reek, rails_best_practices
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

let g:ale_fix_on_save = 0
let g:ale_lint_on_enter = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_set_highlights = 1
