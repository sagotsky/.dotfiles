if ( $TERM != 'linux')          "don't break vim in vterms
  set t_Co=256                "ensures 256 color
  colorscheme railscasts
  "execute "silent !TERM=xterm xtermcontrol --bg 'rgb:20/20/20'"
  " augroup autocom
  "   autocmd!
  "   autocmd VimLeave * !TERM=xterm xtermcontrol --bg rgb:0000/0000/0000
  " augroup END

  hi Normal ctermbg=235
  hi Type ctermfg=121
  hi CursorLineNr ctermbg=235
  highlight Search 		ctermfg=white ctermbg=237 cterm=none
  highlight VertSplit		ctermfg=234 ctermbg=235
  " hi Visual      ctermbg=236
  hi Pmenu                     ctermfg=gray ctermbg=235 gui=NONE
  hi PmenuSel                  ctermfg=white ctermbg=236 gui=NONE
  hi TabLine  ctermbg=233 cterm=bold term=bold ctermfg=236
  hi TabLineSel  ctermbg=235 cterm=bold term=bold
  hi TabLineFill  ctermfg=233

  hi SignColumn guibg=#202020 ctermbg=235

  hi IndentGuidesOdd  guibg=#202020   ctermbg=235
  hi IndentGuidesEven guibg=#2a2a2a ctermbg=234

  " hide tilde on blank lines
  hi EndOfBuffer ctermfg=bg

  " syntastic
  hi SyntasticErrorSign ctermbg=NONE ctermfg=white
  hi SyntasticWarningSign ctermbg=NONE ctermfg=242

  hi CursorLine ctermbg=236
  augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
  augroup END

  syn match Todo /skip/


  "trailing-whitespace
  hi ExtraWhitespace ctermbg=black

  " somehow, setting these is clearing them.  weird.
  hi ALEWarningSign ctermfg=240
  hi ALEErrorSign ctermfg=196

  " error position should underline, but not change color
  hi clear SpellCap
  hi SpellCap gui=underline cterm=underline

  hi DiffAdd    guibg=#202020 ctermbg=235 ctermfg=green
  hi DiffChange guibg=#202020 ctermbg=235 ctermfg=yellow
  hi DiffDelete guibg=#202020 ctermbg=235 ctermfg=red
endif

