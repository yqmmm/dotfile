" =============================================================================
" # Keymap
" =============================================================================
let mapleader = "\<Space>"
"let mapleader=";"
nnoremap <leader><leader> <c-^>

"imap jj <Esc>
nnoremap <C-e> 2<C-e>
nnoremap <C-y> 2<C-y>

" Ctrl+h to stop searching
vnoremap <C-h> :nohlsearch<cr>
nnoremap <C-h> :nohlsearch<cr>

" vim-smooth-scroll
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 9, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 9, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 9, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 9, 4)<CR>

" =============================================================================
" # Editor settings
" =============================================================================

"autocmd BufWritePost ${MYVIMRC} source ${MYVIMRC}  " immediately source .vimrc
set mouse=a

filetype plugin on
map <left> ^
map <right> $

" color
set t_Co=256

" buffers
set wildchar=<Tab> wildmenu wildmode=full
map <C-p> :Files<CR>
nnoremap <up> :bp<CR>
nnoremap <down> :bn<CR>
map <C-p> :Files<CR>
nmap <leader>; :Buffers<CR>

set backspace=2

" autocmd vimenter * NERDTree | wincmd p
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>
let NERDTreeWinPos="right"
let NERDTreeQuitOnOpen=1
let NERDTreeDirArrows=1

colorscheme default

set nocompatible              " be iMproved, required
filetype off                  " required
set hlsearch		      " highlight render search result, use :noh to close
set incsearch		      " search in real time
set ignorecase		      " search ignore case
set wildmenu		      " vim command auto complete

set clipboard=unnamed

set laststatus=2
"set ruler
set number
"set cursorline

syntax enable
syntax on

set ts=4 sw=4 et
let g:indent_guides_start_level=1
let g:indent_guides_guide_size=1
let g:indent_guides_enable_on_vim_startup=1

filetype indent on
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4

set foldmethod=syntax
set nofoldenable

map <Leader> <Plug>(easymotion-prefix)

" =============================================================================
" # Plugin
" =============================================================================
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'nathangrigg/vim-beancount'
Plug 'wakatime/vim-wakatime'
Plug 'terryma/vim-smooth-scroll'
Plug 'easymotion/vim-easymotion'
Plug 'itchyny/lightline.vim'
call plug#end()

filetype plugin indent on    " required
