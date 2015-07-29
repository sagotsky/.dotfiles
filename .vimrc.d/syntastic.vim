let g:syntastic_auto_loc_list=1
let g:syntastic_coffee_coffeelint_args = "--file ".$HOME."/.coffeelint.json"
let g:syntastic_cjsx_checkers=[]
let g:syntastic_ruby_checkers = ['mri'] ", 'rubocop']
let g:syntastic_ruby_rubocop_exec = $HOME . "/bin/rubocop"
let g:syntastic_disabled_filetypes = ['sql']
"
let g:syntastic_quiet_messages = {
  \ 'regex': '\(Prefer single-quoted\|Space missing inside\|Space inside . missing\|too many lines\|too long\)'}
