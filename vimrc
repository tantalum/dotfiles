call plug#begin('~/.vim/plugged')

" Color Schemes
Plug 'joshdick/onedark.vim'

" Vim Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Fining Files
Plug 'ctrlpvim/ctrlp.vim'

" Rainbow CSV
Plug 'mechatroner/rainbow_csv'

call plug#end()

" My configuration
set tabstop=4
set shiftwidth=4
set smartindent
set expandtab
set ignorecase
set smartcase

" filetypes
syntax on
filetype plugin on
filetype indent on

" File encoding
set encoding=utf-8

" Colorscheme
colorscheme default

" Show line numbers
set number

" Special file handeling
au BufNewFile,BufRead *.md set filetype=markdown
au BufNewFile,BufRead *.md set spell
au BufNewFile,BufRead *.md set wrap
au BufNewFile,BufRead *.md set linebreak
