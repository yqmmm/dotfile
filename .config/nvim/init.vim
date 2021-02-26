let mapleader = "\<Space>"
let maplocalleader = "\\"

" Copy whole file content
command! Ca execute "%y+"

let g:python3_host_prog = "/Users/Quack/.asdf/shims/python"

" Learn Vimscript the Hard Way ========= {{{
nnoremap - ddp
nnoremap _ ddkP

inoremap <c-u> <esc>viwUi

nnoremap <leader>ev :tab new $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

iabbrev @@ im.qianmian.yu@gmail.com

"Add a mapping to your ~/.vimrc file that opens the previous buffer in a split of your choosing (vertical/horizontal, above/below/left/right).

" }}}


" =============================================================================
" # Keymap
" =============================================================================
augroup text_file
    autocmd!
    autocmd FileType tex      nnoremap j gj
    autocmd FileType tex      nnoremap k gk
    autocmd FileType markdown nnoremap j gj
    autocmd FileType markdown nnoremap k gk
augroup END

au BufNewFile,BufRead Dockerfile* set filetype=Dockerfile


" Vimscript file settings {{{
augroup vimscript
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END
" }}}


" Mappings {{{
nnoremap <C-e> 2<C-e>
nnoremap <C-y> 2<C-y>

" Search
vnoremap <C-h> :nohlsearch<cr>
nnoremap <C-h> :nohlsearch<cr>
nnoremap <BS>  :nohlsearch<cr>
nnoremap /     /\v

nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>

nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>

map <left> ^
map <right> $
nnoremap <leader>w :w<CR>

noremap <leader>j <c-^>

" buffers
set wildchar=<Tab> wildmenu wildmode=full
nnoremap <up> :bp<CR>
nnoremap <down> :bn<CR>

" tabs
nnoremap <leader>i gT<CR>
nnoremap <leader>o gt<CR>

" Jump List
nnoremap <leader>[ <c-O>
nnoremap <leader>] <c-I>

" clipboard
noremap <Leader>y "*y
noremap <Leader>p "*p

nnoremap <leader>l :silent make\|redraw!\|cc<CR>

nnoremap <F6> :exe ':silent !open ''%'''<CR>

" Help Page
cabbrev help tab help

" Insert time
inoremap <F5> <C-R>=strftime("%F")<CR>
" }}}


" Basic Settings {{{
" Hybrid line number in different mode
" see https://github.com/jeffkreeftmeijer/vim-numbertoggle
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
set laststatus=2
set number
set tabstop=2 shiftwidth=2 expandtab
set softtabstop=2
" set foldmethod=syntax
set nofoldenable
"set ruler
"set cursorline
" }}}

" Helper Functions {{{

" Highlight trailing spaces in file
" Use `:match none` to turn it off
function TrailingSpace()
  match Error /\v\s+$/
endfunction
" }}}

" # Plugin settings {{{
" =============================================================================
" LeaderF Settings {{{
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
" }}}

" NERDTree Settings {{{
" close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <F1> :NERDTreeToggle<CR>
let NERDTreeWinPos="left"
let NERDTreeQuitOnOpen=1
let NERDTreeDirArrows=1
" Open NERDTree on startup if no file is specified
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" }}}

" TagBar
nmap <F7> :TagbarToggle<CR>

" Coc.nvim Settings ------------------------------------------------------ {{{
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
" }}}

let g:rainbow_active = 1

" vimtex Settings {{{
let g:tex_flavor='latex'
let g:vimtex_indent_bib_enabled=0
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
" }}}

" gutentags Settings {{{
" gutentags settings: enable for some dirs
" (https://github.com/ludovicchabant/vim-gutentags/issues/82)

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
" }}}

" }}}

" wiki.vim Settings {{{
let g:wiki_file_open = 'WikiFileOpen'

function! WikiFileOpen(...) abort dict
  if self.path =~# 'png$'
    silent execute '!open' fnameescape(self.path) '&'
    return 1
  endif

  return 0
endfunction
" }}}


" # Plugin {{{
" =============================================================================
call plug#begin('~/.vim/plugged')
" Vim Enhancement
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
" Plug 'easymotion/vim-easymotion'
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
" Plug 'frazrepo/vim-rainbow'
Plug 'airblade/vim-gitgutter'
" Plug 'Yggdroot/indentLine'

" Fuzzy Finder
Plug 'airblade/vim-rooter'
Plug 'Yggdroot/LeaderF', { 'do': './install.sh' }
Plug 'brooth/far.vim'

" Language Specific
Plug 'lervag/vimtex'
Plug 'cespare/vim-toml'
Plug 'neoclide/coc.nvim'
Plug 'majutsushi/tagbar'
Plug 'plasticboy/vim-markdown'
Plug 'nathangrigg/vim-beancount'
" Plug 'chiel92/vim-autoformat'
" Plug 'wlangstroth/vim-racket', { 'for': 'racket'}
" Plug 'ludovicchabant/vim-gutentags'
Plug 'lervag/wiki.vim'
" Plug 'habamax/vim-asciidoctor'

" Plug 'file::///Users/Quack/.vim/plugged/potion'
call plug#end()

filetype plugin indent on    " required
" }}}

runtime! userautoload/*.vim
