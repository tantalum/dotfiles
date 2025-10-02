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

" Rust
Plug 'rust-lang/rust.vim'
Plug 'dense-analysis/ale'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Auto Completion
" Plug 'Valloric/YouCompleteMe'

call plug#end()

" My configuration
set tabstop=4
set shiftwidth=4
set smartindent
set expandtab
set ignorecase
set smartcase
set nocompatible
set cursorline

" filetypes
syntax on
filetype plugin on
filetype indent on

" File encoding
set encoding=utf-8

" Colorscheme
if !empty(getcompletion('sorbet', 'color'))
    colorscheme sorbet
endif

" Show line numbers
set number

" 4 lines under scorll
set scrolloff=4

" Special file handeling
au bufnewfile,bufread *.md set filetype=markdown
au bufnewfile,bufread *.md set spell
au bufnewfile,bufread *.md set wrap
au bufnewfile,bufread *.md set linebreak

au bufnewfile,bufread *.markdown set filetype=markdown
au bufnewfile,bufread *.markdown set spell
au bufnewfile,bufread *.markdown set wrap
au bufnewfile,bufread *.markdown set linebreak

au bufnewfile,bufread *.txt set filetype=markdown
au bufnewfile,bufread *.txt set spell
au bufnewfile,bufread *.txt set wrap
au bufnewfile,bufread *.txt set linebreak
