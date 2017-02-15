let mapleader=" "

nnoremap <leader>p          :CtrlP <cr>
nnoremap <leader>b          :CtrlPBuffer <cr>
nnoremap <leader>r          :CtrlPMRU<cr>
nnoremap <leader>f          :CtrlPFunky<cr>
nnoremap <leader>F          :execute 'CtrlPFunky ' . expand('<cword>')<cr>
nnoremap <leader>/          :Grepper -tool ag -open -switch <cr>
nnoremap <leader>?          :Grepper -tool ag -open -switch -cword -noprompt<cr>
nnoremap <leader>nt         :tabnew<cr>
nnoremap <leader>NT         :tabnew %<cr>

nnoremap <leader>wh         :vertical resize -10<cr>
nnoremap <leader>wl         :vertical resize +10<cr>
nnoremap <leader>wj         :resize +10<cr>
nnoremap <leader>wk         :resize -10<cr>

nnoremap <leader>gs         :Gstatus<cr>
nnoremap <leader>gc         :Gcommit<cr>
nnoremap <leader>gB         :Gblame<cr>
nnoremap <leader>gh         :Gbrowse<cr>
nnoremap <leader>gp         :Git pull<cr>
nnoremap <leader>gP         :Git pp<cr>

nnoremap ]l                 :lnext<cr>
nnoremap [l                 :lprevious<cr>

" Y copies to x11 clipboard
vnoremap Y  "+y

map <F7> :set invspell<CR>
map <F6> :set invwrap<CR>
map <F8> :set invnumber<CR>

map <leader>s :e ~/.vimrc.d/ <cr> :vs <cr> :e ~/.vimrc<cr>
map <leader>S :so ~/.vimrc<cr>
nnoremap <leader>ll         :lclose<cr>:cclose<cr>:pclose<cr>
"noremap <ENTER> : this is fucked up in quickfix

" neovim's parses ^h as backspace, so bind bakcspace to pane left.
nnoremap <silent> <BS> :TmuxNavigateLeft<cr>
:com! Sudow !sudo tee %
:com! W w
:com! Q q
:com! Bd bd