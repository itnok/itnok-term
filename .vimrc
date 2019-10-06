" Load Powerline for Python3.x
exe 'set rtp+=' . expand('${POWERLINE_PY_PKG_DIR}/powerline/bindings/vim/')
" Set path to vim data/config directory
let vim_data_path='~/.vim'
" Set path to vim plugins
let vim_plugins_path=vim_data_path . '/plugged'
" Always show statusline
set laststatus=2
" Activate Deoplete autocompletion plugin
" (only if vim8 & python 3.6)
if v:version >= 800
  if has("python3")
    python3 import vim; from sys import version_info as v; vim.command('let python_version=%d' % (v[0] * 100 + v[1]))
  else
    let python_version=0
  endif
  if python_version >= 306
    " Try to auto-install vim-plug if missing
    let autoload_vimplug_path=vim_data_path . '/autoload/plug.vim'
    if empty(glob(autoload_vimplug_path))
      silent exe '!curl -fL --create-dirs -o ' . autoload_vimplug_path .
        \ ' https://raw.github.com/junegunn/vim-plug/master/plug.vim'
      execute 'source ' . fnameescape(autoload_vimplug_path)
    endif
    unlet autoload_vimplug_path
    " Specify a directory for plugins
    "   - Avoid using standard Vim directory names like 'plugin'
    call plug#begin(vim_plugins_path)

    " List plugins to load
    " making sure single quotes are always used
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'terryma/vim-multiple-cursors'
    Plug 'tpope/vim-eunuch'
    Plug 'airblade/vim-gitgutter'

    " END of plugin sysetm initialization
    call plug#end()
    " Autoinstall plugins
    autocmd VimEnter *
          \  if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
          \|   PlugInstall --sync | q
          \| endif
    " Use tab for trigger completion with characters ahead and navigate.
    " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
    inoremap <silent><expr> <TAB>
          \ pumvisible() ? "\<C-n>" :
          \ <SID>check_back_space() ? "\<TAB>" :
          \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    " Use <c-space> to trigger completion.
    inoremap <silent><expr> <c-space> coc#refresh()

  endif
endif
" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256
" Set the encoding for display
set encoding=utf-8
" Set encoding for writing to file
set fileencoding=utf-8
" Always shows line number
set number
" Set line numbers color to grey #585858
highlight LineNr cterm=NONE ctermfg=240 ctermbg=233
" Set current line number to bold white #eeeeee
highlight CursorLine   cterm=NONE
highlight CursorLineNR cterm=bold ctermfg=255
set cursorline
" Always highlight search results
set hlsearch
" Set search highlight to yellow #87ff00
highlight Search ctermbg=118 ctermfg=235
" Set highlighted text in visual mode to grey
highlight Visual cterm=bold ctermbg=DarkGrey ctermfg=NONE
if !empty(glob(vim_plugins_path . '/vim-gitgutter/autoload/gitgutter.vim'))
  " Customize vim-gitgutter colors
  let g:gitgutter_override_sign_column_highlight = 0
  let g:gitgutter_sign_added = '++'
  let g:gitgutter_sign_modified = '~~'
  let g:gitgutter_sign_removed = '--'
  let g:gitgutter_highlight_linenrs = 1
  set signcolumn=yes
  highlight SignColumn      cterm=bold ctermbg=235
  highlight GitGutterAdd    cterm=bold ctermbg=235 ctermfg=2   " green  #008000
  highlight GitGutterChange cterm=bold ctermbg=235 ctermfg=11  " yellow #ffff00
  highlight GitGutterDelete cterm=bold ctermbg=235 ctermfg=9   " red    #ff0000
  " Update every 1.5s
  set updatetime=1500
endif
" Set Syntax Highlight ON
syntax on
" Set Hack font-family if vim is run in a GUI
if has('gui_running')
  set guifont=Hack:h14
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
" Set clipboard to use both system and X one
set clipboard=unnamed,unnamedplus
" On exit copy the default buffer to clipboard
autocmd VimLeave * call system("echo -n $'" . escape(getreg(), "'") . "' | xclip -i -f -selection primary | xclip -i -selection clipboard")

