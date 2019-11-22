let mapleader=" "

nnoremap <silent><leader>/          :Ag <cr>
nnoremap <leader>?          :Ag <C-R><C-W><cr>
nnoremap <leader>p          0:Files <cr>
nnoremap <leader>b          0:Buffers <cr>
nnoremap <leader>r          0:History<cr>
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
nnoremap <leader>gl         :BCommits!<cr>

nnoremap ]l                 :lnext<cr>
nnoremap [l                 :lprevious<cr>

nnoremap ]q                 :colder<cr>
nnoremap [q                 :cnewer<cr>
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

" dispatch for test running
nnoremap <leader>t :Dispatch rspec %<CR>
nnoremap <leader>T :Dispatch rspec %:<C-r>=line('.')<CR><CR>

" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)

" coc.nvim
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> <leader>F <Plug>(coc-fix-current)

" Use `lp` and `ln` for navigate diagnostics
nmap <silent> <leader>lp <Plug>(coc-diagnostic-prev)
nmap <silent> <leader>ln <Plug>(coc-diagnostic-next)

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
" ctrl-c doesn't clear floating window.  esc does.
imap <C-c> <Esc>
