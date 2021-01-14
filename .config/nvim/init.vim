let mapleader = "\<Space>"
let maplocalleader = "\\"

" Copy whole file content
command! Ca execute "%y+"

let g:python3_host_prog = "/Users/Quack/.asdf/shims/python"
"
" =============================================================================
" # Keymap
" =============================================================================
autocmd FileType tex nnoremap j gj
autocmd FileType tex nnoremap k gk
autocmd FileType markdown nnoremap j gj
autocmd FileType markdown nnoremap k gk

nnoremap <C-e> 2<C-e>
nnoremap <C-y> 2<C-y>

" Ctrl+h & Backspace to stop searching
vnoremap <C-h> :nohlsearch<cr>
nnoremap <C-h> :nohlsearch<cr>
nnoremap <BS> :nohlsearch<cr>

nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>

nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>

map <left> ^
map <right> $
nmap <leader>w :w<CR>

" buffers
set wildchar=<Tab> wildmenu wildmode=full
nnoremap <up> :bp<CR>
nnoremap <down> :bn<CR>

" tabs
nnoremap <leader>i gT<CR>
nnoremap <leader>o gt<CR>

" clipboard
noremap <Leader>y "*y
noremap <Leader>p "*p

nnoremap <leader>l :silent make\|redraw!\|cc<CR>

" Help Page
" :cabbrev help tab help
cabbrev help tab help

" let g:rooter_change_directory_for_non_project_files = 'current'

" LeaderF
let g:Lf_WindowPosition = 'popup'
" let g:Lf_UseVersionControlTool = 0
let g:Lf_ShowDevIcons = 0
let g:Lf_ShortcutF = "<C-p>"
let g:Lf_ShortcutB = "<C-a>"
noremap <leader>m :<C-U><C-R>=printf("Leaderf mru %s", "")<CR><CR>
noremap <leader>f :Leaderf rg<CR>
noremap <leader>b :Leaderf buffer<CR>
noremap <leader>a :Leaderf cmdHistory<CR>
xnoremap gf :<C-U><C-R>=printf("Leaderf! rg -F -e %s ", leaderf#Rg#visual())<CR>
let g:Lf_CommandMap = {'<C-J>': ['<C-J>', '<Down>'], '<C-K>': ['<C-K>', '<Up>']}
let g:Lf_PreviewInPopup = 1

" TagBar
nmap <F7> :TagbarToggle<CR>

" im-select
" let g:im_select_enable_focus_events = 0

" =============================================================================
" # Editor settings
" =============================================================================
" Hybrid line number in different mode, see https://github.com/jeffkreeftmeijer/vim-numbertoggle
" augroup numbertoggle
"   autocmd!
"   autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu   | endif
"   autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu | set nornu | endif
" augroup END


" vim-smooth-scroll
" noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 9, 2)<CR>
" noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 9, 2)<CR>
" noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 9, 4)<CR>
" noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 9, 4)<CR>

"autocmd BufWritePost ${MYVIMRC} source ${MYVIMRC}  " immediately source .vimrc
set mouse=a
set autowrite

" color
colorscheme default
filetype plugin on
filetype indent on
syntax enable
syntax on
set t_Co=256
set nocompatible              " be iMproved, required
set backspace=2
set hlsearch		      " highlight render search result, use :noh to close
set incsearch		      " search in real time
set ignorecase		      " search ignore case
set wildmenu		      " vim command auto complete
" set clipboard=unnamed
set ts=4 sw=4 et
set laststatus=2
set number
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set foldmethod=syntax
set nofoldenable
"set ruler
"set cursorline

au BufNewFile,BufRead Dockerfile* set filetype=Dockerfile

" =============================================================================
" # Plugin settings
" =============================================================================
" NERDTree
" close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <F1> :NERDTreeToggle<CR>
let NERDTreeWinPos="left"
let NERDTreeQuitOnOpen=1
let NERDTreeDirArrows=1
" Open NERDTree on startup if no file is specified
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

let g:indent_guides_start_level=1
let g:indent_guides_guide_size=1
let g:indent_guides_enable_on_vim_startup=1

" Coc Completion
" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

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

" Use <c-.> to trigger completion.
inoremap <silent><expr> <c-.> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

let g:rainbow_active = 1

" Markdown
" set concealcursor=i
" set g:indentLine_concealcursor="nc"

" vimtex
let g:tex_flavor='latex'
let g:vimtex_quickfix_mode=0
let g:vimtex_texcount_custom_arg='-incbib -ch' " include bibtex and count chinese characters
let g:vimtex_compiler_latexmk = {
    \ 'options' : [
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \   '-shell-escape',
    \ ],
    \}


noremap <leader>j <c-^>

" Beancount shortcut
inoremap <F5> <C-R>=strftime("%F")<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" gutentags settings: enable for some dirs
" (https://github.com/ludovicchabant/vim-gutentags/issues/82)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" let g:gutentags_enabled_dirs = []
" let g:gutentags_enabled_user_func = 'CheckEnabledDirs'

" function! CheckEnabledDirs(file)
"     let file_path = fnamemodify(a:file, ':p:h')

"     try
"         let gutentags_root = gutentags#get_project_root(file_path)
"         if filereadable(gutentags_root . '/.withtags')
"             return 1
"         endif
"     catch
"     endtry

"     for enabled_dir in g:gutentags_enabled_dirs
"         let enabled_path = fnamemodify(enabled_dir, ':p:h')

"         if match(file_path, enabled_path) == 0
"             return 1
"         endif
"     endfor

"     return 0
" endfunction

" wiki
let g:wiki_root = '~/notes'
let g:wiki_filetypes = ['wiki', 'md']


" =============================================================================
" # Plugin
" =============================================================================
call plug#begin('~/.vim/plugged')
Plug 'lervag/vimtex'
" Vim Enhancement
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'easymotion/vim-easymotion'
Plug 'scrooloose/nerdtree'
Plug 'wakatime/vim-wakatime'
Plug 'ybian/smartim'
" Plug 'tmux-plugins/vim-tmux-focus-events' " For vim-im-select to work under tmux
" Plug 'brglng/vim-im-select'
Plug 'tpope/vim-eunuch'
" Plug 'terryma/vim-smooth-scroll'

" Text Object
" Plug 'kana/vim-textobj-user'
" Plug 'sgur/vim-textobj-parameter'
" Plug 'kana/vim-textobj-function', { 'for':['c', 'cpp', 'vim', 'java', 'rust'] }

" GUI Enhancement
Plug 'itchyny/lightline.vim'
Plug 'frazrepo/vim-rainbow'
Plug 'airblade/vim-gitgutter'
" Plug 'Yggdroot/indentLine'

" Fuzzy Finder
Plug 'airblade/vim-rooter'
Plug 'Yggdroot/LeaderF', { 'do': './install.sh' }

" Language Specific
Plug 'cespare/vim-toml'
Plug 'neoclide/coc.nvim'
Plug 'majutsushi/tagbar'
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'nathangrigg/vim-beancount'
" Plug 'chiel92/vim-autoformat'
" Plug 'wlangstroth/vim-racket', { 'for': 'racket'}

" Plug 'ludovicchabant/vim-gutentags'
Plug 'lervag/wiki.vim'
call plug#end()

filetype plugin indent on    " required

runtime! userautoload/*.vim
