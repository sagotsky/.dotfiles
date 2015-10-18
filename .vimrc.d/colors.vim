if ( $TERM != 'linux')          "don't break vim in vterms
  set t_Co=256                "ensures 256 color
  colorscheme railscasts 
  "execute "silent !TERM=xterm xtermcontrol --bg 'rgb:20/20/20'"
  " augroup autocom
  "   autocmd!
  "   autocmd VimLeave * !TERM=xterm xtermcontrol --bg rgb:0000/0000/0000
  " augroup END

  "highlight linenr 		ctermfg=darkgray	
  "highlight CursorLine 	ctermbg=235 cterm=bold 
  "    highlight String 		ctermfg=green 
  "    highlight Constant 		ctermfg=red 
  "highlight Comment 		ctermfg=darkgray
  highlight Search 		ctermfg=white ctermbg=237 cterm=none
  "    highlight Todo		ctermfg=21 ctermbg=11
  "highlight StatusLine	cterm=bold ctermfg=white ctermbg=black
  "highlight StatusLineNC	cterm=bold ctermfg=darkgray ctermbg=black
  highlight VertSplit		ctermfg=234 ctermbg=235
  "highlight SpellBad          ctermfg=229 cterm=underline
  "hi normal ctermbg=black
  hi Visual      ctermbg=236
  hi Pmenu                     ctermfg=gray ctermbg=235 gui=NONE
  hi PmenuSel                  ctermfg=white ctermbg=236 gui=NONE
  hi Comment      cterm=italic gui=italic
  hi TabLineFile  ctermbg=0 cterm=bold term=bold 

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


  " mark text beyond 120c with italic.  not sure if this is more or less
  " obnoxious than a color
  hi ColorColumn cterm=italic 
  
  " syntastic
  hi SyntasticErrorSign ctermbg=NONE ctermfg=white
  hi SyntasticWarningSign ctermbg=NONE ctermfg=242
endif
