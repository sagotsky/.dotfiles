let g:syntastic_auto_loc_list=1
"let g:syntastic_quiet_messages = {'level': 'warnings'}
let g:syntastic_coffee_coffeelint_args = "--file ".$HOME."/.coffeelint.json"
"let g:syntastic_filetype_map = { "mustache": "handlebars" }
let g:syntastic_cjsx_checkers=[]
let g:syntastic_ruby_checkers = ['mri', 'rubocop']
let g:syntastic_ruby_rubocop_exec = $HOME . "/bin/rubocop"
