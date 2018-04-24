ab pry binding.pry
ab rpry binding.remote_pry
ab kpry ::Kernel.binding.pry
ab raise raise StandardError,
ab rescue rescue StandardError
highlight RubyWords ctermbg=black ctermfg=green
match RubyWords /binding\.pry/
let g:ruby_indent_assignment_style = 'variable'
