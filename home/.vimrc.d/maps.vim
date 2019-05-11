let mapleader=" "

nnoremap <leader>p          0:GFiles <cr>
nnoremap <leader>P          0:Files <cr>
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
nnoremap <leader>gl         :GitMessenger<cr>

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

tnoremap <C-w>h <C-\><C-n><C-w>h
tnoremap <C-w>j <C-\><C-n><C-w>j
tnoremap <C-w>k <C-\><C-n><C-w>k
tnoremap <C-w>l <C-\><C-n><C-w>l

" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)

" coc.nvim
nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gt <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gR <Plug>(coc-rename)
nmap <silent> <leader>F <Plug>(coc-fix-current)
