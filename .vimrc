" Load Powerline for Python3.x
exe 'set rtp+=' . expand('~/.local/lib/python3*/site-packages/powerline/bindings/vim/')
" Always show statusline
set laststatus=2
" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256
" Set the encoding for display
set encoding=utf-8
" Set encoding for writing to file
set fileencoding=utf-8
" Always shows line number
set number
" Set line numbers color to grey
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
" Always highlight search results
set hlsearch
" Set search highlight to yellow
highlight Search guibg=Yellow3
" Set highlighted text in visual mode to grey
highlight Visual cterm=bold ctermbg=DarkGrey ctermfg=NONE
" Set Syntax Highlight ON
syntax on
" Set Hack font-family if vim is run in a GUI
if has('gui_running')
  set guifont=Hack:h12
endif
" The width of a TAB is set to 2. Still it is a \t. It is just that
" Vim will interpret it to be having a width of 2.
set tabstop=2
" Indents will have a width of 2
set shiftwidth=2
" Sets the number of columns for a TAB
set softtabstop=2
" Expand TABs to spaces
set expandtab
" Always remove trailing spaces on saving
autocmd BufWritePre * %s/\s\+$//e
" Activate mouse support
set mouse=a
