if has('nvim')
    if exists(':GuiFont')
        GuiFont! Monospace:h10
    endif
else
    set guifont=Monospace\ 10
endif

" Remve the toolbar and menubar
set guioptions-=m
set guioptions-=T

" Colorscheme
colorscheme sorbet

" Initial window size
set lines=40 columns=120
