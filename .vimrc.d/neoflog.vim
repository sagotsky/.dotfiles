" https://github.com/malept/vim-flog/blob/master/plugin/show_complexity.rb
" File:        neoflog.vim
" Description: Ruby cyclomatic complexity analizer
" Author:      Jon Sagotsky <sagotsky@gmail.com>
" Licence:     WTFPL
" Version:     0.0.1

finish
" if !has('signs') || !has('ruby') file_exists flog
"    finish
" endif

let s:medium_limit     = 10
let s:high_limit       = 20
let s:hide_low         = 0
let s:hide_medium      = 0

if exists("g:flog_hide_low")
  let s:hide_low = g:flog_hide_low
endif

if exists("g:flog_hide_medium")
  let s:hide_medium = g:flog_hide_medium
endif

if exists("g:flog_medium_limit")
  let s:medium_limit = g:flog_medium_limit
endif

if exists("g:flog_high_limit")
  let s:high_limit = g:flog_high_limit
endif

"exec expand("rubyfile <sfile>:p:h/flog.rb")

function! ShowComplexity()
  "exec "rubyfile " . findfile("plugin/show_complexity.rb", &rtp)
endfunction

function! HideComplexity()
  "exec "rubyfile " . findfile("plugin/hide_complexity.rb", &rtp)
endfunction

function! FlogDisable()
  let g:flog_enable = 0
  call HideComplexity()
endfunction
command! FlogDisable call FlogDisable()

function! FlogEnable()
  let g:flog_enable = 1
  call ShowComplexity()
endfunction
command! FlogEnable call FlogEnable()

function! FlogToggle()
  if exists("g:flog_enable") && g:flog_enable
    call FlogDisable()
  else
    call FlogEnable()
  endif
endfunction
command! FlogToggle call FlogToggle()

if !exists("g:flog_enable") || g:flog_enable
  au BufReadPost,BufWritePost,FileReadPost,FileWritePost *.rb call ShowComplexity()
endif

sign define NeoflogMedium text=•
sign define NeoflogHigh text=‣

function! FlogScoreLineNr(line)
  let a = split(a:line, ':')
  return [a[0], a[-1]]
endfunction

function! FlogSigns(score_line)
  " how to get an id?  (v:key gives array index.  maybe use that and then
  " clear when done?)
  let line = a:score_line[1]
  let id = 124
  exec 'sign place ' . id . ' line=' . line . ' name=NeoflogHigh buffer=' . bufnr('%')
endfunction

function! FlogClearSigns()
  let buffer = bufnr('%')
  redir => all_signs
    silent 'sign place buffer=' . buffer
  redir END 
endfunction

sign unplace 124 
sign unplace 123 
"exec 'sign place 999888 line=61 name=neoflog-high buffer='.bufnr('')
silent let flogged = system('flog ' . expand('%'). ' 2>/dev/null | grep ' . expand('%'))
silent let score_line = map(split(flogged, '\n'), 'FlogScoreLineNr(v:val)')
silent let sign_cmds = map(deepcopy(score_line), 'FlogSigns(v:val)')

