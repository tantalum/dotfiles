" filetypes
filetype plugin on
filetype indent on
" ~/.vimrc ends here

" My configuration
set tabstop=4
set shiftwidth=4
set smartindent
set expandtab
set ignorecase
set smartcase

" File encoding
set encoding=utf-8

" Colorscheme
colorscheme elflord

" Show line numbers
set number

" Key Mappings
map <C-f> :NERDTreeToggle<CR>

" Special file handeling
au BufNewFile,BufRead *.md set filetype=markdown
au BufNewFile,BufRead *.md set spell
au BufNewFile,BufRead *.md set wrap
au BufNewFile,BufRead *.md set linebreak
