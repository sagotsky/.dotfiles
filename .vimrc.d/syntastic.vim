let g:syntastic_auto_loc_list=0
let g:syntastic_check_on_open=1
let g:syntastic_coffee_coffeelint_args = "--file ".$HOME."/.coffeelint.json"
let g:syntastic_cjsx_checkers=[]
let g:syntastic_ruby_checkers = ['mri', 'flog'] ", 'rubocop']
let g:syntastic_ruby_rubocop_exec = $HOME . "/bin/rubocop"
let g:syntastic_ruby_flog_args = '--details'
let g:syntastic_disabled_filetypes = ['sql']
let g:syntastic_always_populate_loc_list=1 " lprev/lnext work without list now

let g:syntastic_ruby_flog_threshold_warning=25 
let g:syntastic_ruby_flog_threshold_error=50 

let g:syntastic_quiet_messages = {
  \ 'regex': '\(interpreted as argument\|Prefer single-quoted\|Space missing inside\|Space inside . missing\|too many lines\|too long\)'}


let g:syntastic_error_symbol = "▶"
let g:syntastic_warning_symbol = "▶"
let g:syntastic_style_error_symbol = "•"
let g:syntastic_style_warning_symbol = "•"
