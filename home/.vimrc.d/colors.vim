if ( $TERM != 'linux')          "don't break vim in vterms
  set t_Co=256                "ensures 256 color
  colorscheme railscasts

  hi Normal ctermbg=234
  hi Type ctermfg=121
  hi CursorLineNr ctermbg=234
  hi LineNr ctermfg=239 ctermbg=234
  highlight Search 		ctermfg=white ctermbg=237 cterm=none
  highlight VertSplit		ctermfg=233 ctermbg=234

  hi Pmenu                     ctermfg=gray ctermbg=235 gui=NONE
  hi PmenuSel                  ctermfg=white ctermbg=235 gui=NONE
  hi PmenuThumb ctermbg=235 ctermfg=234

  hi TabLine  ctermbg=232 cterm=bold term=bold ctermfg=235
  hi TabLineSel  ctermbg=234 cterm=bold term=bold
  hi TabLineFill  ctermfg=232

  " lightline hijacks the tabline.  override its color scheme
  let s:palette = g:lightline#colorscheme#jellybeans#palette
  let s:palette.tabline.left =   [  [ 'none', 'none', 239, 'none' ] ]
  let s:palette.tabline.tabsel = [  [ 'none', 'none', 250, 'none', 'bold' ] ]
  let s:palette.tabline.middle = [  [ 'none', 'none', 'blue', 'none' ] ]
  let s:palette.tabline.right =  [  [ 'none', 'none', 239, 'none' ] ]
  unlet s:palette

  hi SignColumn guibg=#202020 ctermbg=234

  hi IndentGuidesOdd  guibg=#202020   ctermbg=234
  hi IndentGuidesEven guibg=#2a2a2a ctermbg=233

  " hide tilde on blank lines
  hi EndOfBuffer ctermfg=bg

  hi CursorLine ctermbg=235
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
  hi SpellBad ctermfg=229

  hi DiffAdd    guibg=#202020 ctermbg=234 ctermfg=green
  hi DiffChange guibg=#202020 ctermbg=234 ctermfg=yellow
  hi DiffDelete guibg=#202020 ctermbg=234 ctermfg=red

  hi LightlineLeft_inactive_0  ctermbg=233
  hi LightlineMiddle_inactive  ctermbg=233
  hi LightlineRight_inactive_0 ctermbg=233 ctermfg=239
  hi LightlineRight_inactive_1 ctermbg=233 ctermfg=239
endif
