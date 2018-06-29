let mapleader=" "


nnoremap <leader>p          0:Files <cr>
nnoremap <leader>b          0:Buffers <cr>
nnoremap <leader>r          0:History<cr>
nnoremap <leader>l          0:Lines!<cr>
nnoremap <leader>L          0:BLines!<cr>
nnoremap <silent><leader>/          :Ag <cr>
nnoremap <leader>?          :Grepper -tool rg -open -switch -cword -noprompt<cr>
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
" P pastes from it
vnoremap P :Paste<cr>

map <F7> :set invspell<CR>
map <F6> :set invwrap<CR>
map <F8> :set invnumber<CR>

map <leader>s :e ~/.vimrc.d/ <cr> :vs <cr> :e ~/.vimrc<cr>
map <leader>S :so ~/.vimrc<cr>
nnoremap <leader>ll         :lclose<cr>:cclose<cr>:pclose<cr>

nnoremap <leader>af       :ALEFix<cr>

" neovim's parses ^h as backspace, so bind bakcspace to pane left.
nnoremap <silent> <BS> :TmuxNavigateLeft<cr>
:com! Sudow !sudo tee %
:com! W w
:com! Q q
:com! Bd bd
