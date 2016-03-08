if ( $TERM != 'linux')          "don't break vim in vterms
  set t_Co=256                "ensures 256 color
  colorscheme railscasts 
  "execute "silent !TERM=xterm xtermcontrol --bg 'rgb:20/20/20'"
  " augroup autocom
  "   autocmd!
  "   autocmd VimLeave * !TERM=xterm xtermcontrol --bg rgb:0000/0000/0000
  " augroup END

  highlight Search 		ctermfg=white ctermbg=237 cterm=none
  highlight VertSplit		ctermfg=234 ctermbg=235
  hi Visual      ctermbg=236
  hi Pmenu                     ctermfg=gray ctermbg=235 gui=NONE
  hi PmenuSel                  ctermfg=white ctermbg=236 gui=NONE
  hi TabLineFile  ctermbg=0 cterm=bold term=bold 
  hi TabLineFill  cterm=none

  " gitgutter
  hi SignColumn guibg=#202020 ctermbg=234
  hi GitGutterAddDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeDeleteDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeLineDefault guibg=#202020 ctermbg=234
  hi GitGutterChangeDeleteDefault guibg=#202020 ctermbg=234
  hi GitGutterDeleteDefault guibg=#202020 ctermbg=234

  hi IndentGuidesOdd  guibg=#202020   ctermbg=235
  hi IndentGuidesEven guibg=#2a2a2a ctermbg=234

  " syntastic
  hi SyntasticErrorSign ctermbg=NONE ctermfg=white
  hi SyntasticWarningSign ctermbg=NONE ctermfg=242

  hi CursorLine ctermbg=236
  augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
  augroup END
endif


