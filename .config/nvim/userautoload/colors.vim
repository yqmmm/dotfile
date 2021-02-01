" Use GUI colorsetting in TUI
" Does not really look coherent with iTerm2.
" set termguicolors

colorscheme zellner

" vim-gitgutter
" let g:gitgutter_signs = 0
let g:gitgutter_enabled = 0
highlight clear SignColumn
noremap <leader>g :GitGutterToggle<CR>
let g:gitgutter_highlight_linenrs=1
highlight GitGutterAddLineNr ctermfg=46 cterm=bold
highlight GitGutterChangeLineNr ctermfg=Red
