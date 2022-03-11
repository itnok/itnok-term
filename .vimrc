" Load Powerline for Python3.x
exe 'set rtp+=' . expand('${POWERLINE_PY_PKG_DIR}/powerline/bindings/vim/')
" Set path to vim data/config directory
let vim_data_path='~/.vim'
" Set path to vim plugins
let vim_plugins_path=vim_data_path . '/plugged'
" Always show statusline
set laststatus=2
" Set default shell to BASH
set shell=bash
" Activate Deoplete autocompletion plugin
" (only if vim8 & python 3.6)
if v:version >= 800
  if (v:version <= 801) && !has('patch1719')
    let g:coc_disable_startup_warning=1
  endif
  if has("python3")
    python3 import vim; from sys import version_info as v; vim.command('let python_version=%d' % (v[0] * 100 + v[1]))
  else
    let python_version=0
  endif
  if python_version >= 306
    " The plugin neoclide/coc.vim might have some
    " troubles finding the right node executable
    " let's give it some help...
    let g:coc_node_path=substitute(system('which node'),'\n$','','')
    " Try to auto-install vim-plug if missing
    let autoload_vimplug_path=vim_data_path . '/autoload/plug.vim'
    if empty(glob(autoload_vimplug_path))
      silent exe '!curl -sfL --create-dirs -o ' . autoload_vimplug_path .
        \ ' https://raw.github.com/junegunn/vim-plug/master/plug.vim'
      execute 'source ' . fnameescape(autoload_vimplug_path)
    endif
    unlet autoload_vimplug_path
    " Specify a directory for plugins
    "   - Avoid using standard Vim directory names like 'plugin'
    call plug#begin(vim_plugins_path)

    " List plugins to load
    " making sure single quotes are always used
    Plug 'dracula/vim', {'as': 'dracula'}
    Plug 'editorconfig/editorconfig-vim'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'terryma/vim-multiple-cursors'
    Plug 'tpope/vim-eunuch'
    Plug 'airblade/vim-gitgutter'
    Plug 'preservim/nerdtree'
          \| Plug 'Xuyuanp/nerdtree-git-plugin'
          \| Plug 'ryanoasis/vim-devicons'
    Plug 'fatih/vim-go', {'do': ':GoUpdateBinaries'}

    " END of plugin sysetm initialization
    call plug#end()
    " Start NERDTree. If a file is specified, move the cursor to its window.
    autocmd StdinReadPre * let s:std_in=1
    " Autoinstall plugins on startup
    autocmd VimEnter *
          \  if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
          \|   PlugInstall --sync | q
          \| endif
          \| NERDTree
          \| if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in')
          \|   execute 'NERDTree' argv()[0]
          \|   wincmd p
          \|   enew
          \|   execute 'cd '.argv()[0]
          \| else
          \|   wincmd p
          \| endif
    " Exit Vim if NERDTree is the only window left.
    autocmd BufEnter *
          \  if tabpagenr('$') == 1 && winnr('$') == 1
          \  && exists('b:NERDTree') && b:NERDTree.isTabTree()
          \|   quit
          \| endif
    " Open the existing NERDTree on each new tab.
    autocmd BufWinEnter * silent NERDTreeMirror
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

    " Make NERDTree show dot-files (hidden)
    let NERDTreeShowHidden=1
    " Load the DevIcon plugin
    let g:webdevicons_enable=1
    " Add the flags to NERDTree
    let g:webdevicons_enable_nerdtree=1
    " The amount of space to use after the DevIcon glyph character (default ' ')
    let g:WebDevIconsNerdTreeAfterGlyphPadding='  '
    " Force extra padding in NERDTree so that the filetype icons line up vertically
    let g:WebDevIconsNerdTreeGitPluginForceVAlign=1
    " Enable open and close folder/directory glyph flags (default 0)
    let g:DevIconsEnableFoldersOpenClose=1
    " Enable NerdFont support for NERDTreeGit (default 0)
    let g:NERDTreeGitStatusUseNerdFonts=1
    " Open files in a new tab on <CR> (instead of in a new panel)
    let NERDTreeCustomOpenArgs={'file': {'where': 't', 'reuse': 'all', 'keepopen': 1, 'stay': 0 }}

    " Normal mode: \n focus on NERDTree
    nnoremap <leader>] :NERDTreeFocus<CR>
    " Normal mode: Ctrl+t toggle NERDTree
    nnoremap <C-t> :NERDTreeToggle<CR>
    " Normal mode: Ctrl+f find current file in NERDTree
    nnoremap <C-f> :NERDTreeFind<CR>

  endif
endif
" Go to tab by number \x (e.g. \3 to go to tab #3)
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>
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
" Set preferred colorscheme to dracula
" (but do not freak out if not there!)
:silent! colorscheme dracula
" Set 'Hack NF' (Hack font with NerdFont-patched!) font-family if vim is run in a GUI
if has('gui_running')
  set guifont='Hack NF':h14
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

" Fn: Create a panel to host a terminal
function! CreateTermPanel(buf, side, size) abort
  " Create a new terminal in no buffer is present just yet
  if a:buf == 0
    term
  else
    execute "sp" bufname(a:buf)
  endif
  " Set the default side to bottom if a wrong/not-existing argument is passed
  if stridx("hjklHJKL", a:side) == -1
    execute "wincmd" "J"
  else
    execute "wincmd" a:side
  endif
  " Horizontal split resize
  if stridx("jkJK", a:side) >= 0
    if ! a:side > 0
      resize 7
    else
      execute "resize" a:size
    endif
    return
  endif
  " Vertical split resize
  if stridx("hlHL", a:side) >= 0
    if ! a:side > 0
      vertical resize 15
    else
      execute "vertical resize" a:size
    endif
  endif
endfunction

" Fn: Toggle a terminal panel created by CreateTermPanel
function! s:ToggleTermPanel(side, size) abort
  let tpbl = []
  let closed = 0
  let tpbl = tabpagebuflist()
  " Hide visible terminals
  for buf in filter(range(1, bufnr('$')), 'bufexists(bufname(v:val)) && index(tpbl, v:val) >= 0')
    if getbufvar(buf, '&buftype') ==? 'terminal'
      silent execute bufwinnr(buf) . "hide"
      let closed += 1
    endif
  endfor
  if closed > 0
    return
  endif
  " Open first hidden terminal
  for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val) < 0')
    if getbufvar(buf, '&buftype') ==? 'terminal'
      call CreateTermPanel(buf, a:side, a:size)
      return
    endif
  endfor
  " Open new terminal
  call CreateTermPanel(0, a:side, a:size)
endfunction

" Toggle terminal - bottom
nnoremap <leader>t :call <SID>ToggleTermPanel('J', 7)<CR>
