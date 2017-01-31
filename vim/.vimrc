runtime! archlinux.vim
filetype plugin indent on
filetype on
filetype plugin on
set nocompatible "Vim, not vi

" Addons settings {{{

execute pathogen#infect()
execute pathogen#helptags()

" Syntastic options
let g:syntastic_cpp_compiler = 'gcc'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++ -Wall -Werror -Wextra'
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_remove_include_errors = 1
let g:syntastic_c_remove_include_errors = 1
let g:syntastic_c_include_dirs = ['../../../includes', '../../includes','../includes','./includes''../../../include', '../../include','../include','./include']

" Nerd commenter
let g:NERDSpaceDelims = 1
let g:NERDTrimTrailingWhitespace = 1

" Promptline
let g:promptline_preset = {
		\'a'    : [ '$USER' ],
		\'b'    : [ promptline#slices#cwd() ],
		\'c'    : [ promptline#slices#vcs_branch() ],
		\'warn' : [ promptline#slices#last_exit_code() ]}
let g:promptline_symbols = {
		\'left' : '',
		\'dir_sep' : '',}

" Lightline
let g:lightline = {
		\ 'colorscheme': 'gruvbox',
		\ 'active': {
		\   'left': [ [ 'mode', 'paste' ], [ 'filename' ], ],
		\   'right': [ [ 'syntastic', 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
		\ },
		\'component': {
		\ 'readonly': '%{&readonly?"":""}',
		\ },
		\ 'component_expand': {
		\   'syntastic': 'SyntasticStatuslineFlag',
		\ },
		\ 'component_type': {
		\   'syntastic': 'error',
		\ },
		\ 'separator': { 'left': '▓▒░', 'right': '░▒▓' 
		\ },
		\ 'subseparator': { 'left': '▒', 'right': '░' 
		\ }
		\ }

augroup AutoSyntastic
  autocmd!
  autocmd BufWritePost *.c,*.cpp call s:syntastic()
augroup END
function! s:syntastic()
  SyntasticCheck
  call lightline#update()
endfunction

" Select expanding regions through spamming v
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)
let g:expand_region_text_objects = {
		\ 'i''' :1,
		\ 'i"'  :1,
		\ 'ip'  :1,
		\ 'iB'  :1,
		\ 'ib'  :1,
		\ 'i[' :1,
		\ }
call expand_region#custom_text_objects({
	\ "\/\\n\\n\<CR>" : 1,
	\ })

" Ctrl-P
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v[\/]\.(git|hg|svn)$',
	\ 'file': '\v\.(exe|so|dll)$',
	\ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
	\ }
let g:ctrlp_switch_buffer = 'Et'
let g:ctrlp_cmd = 'CtrlPMixed'

" }}}
" Colorscheme & Statusline settings {{{

colorscheme gruvbox
set background=dark
hi CursorLine cterm=bold ctermbg=234
let g:gruvbox_termcolors = 256
let g:gruvbox_contrast_dark ='hard'
let g:gruvbox_improved_warnings = 1
let g:gruvbox_italic = 1
"set t_Co=256
set laststatus=2

" }}}
" UI Config {{{

syntax on "Shows syntax
set cursorline "Highlight current line
set showmatch "Shows matching braces
set backspace=indent,eol,start "More powerful backspace
set number "Show line numbers
set cc=80 "Highlights column 80
set whichwrap+=<,>,h,l,[,]
"set nofoldenable "No folders
set foldmethod=marker
"set list listchars=tab:»·,trail:·
set list listchars=tab:▸\ ,trail:·,nbsp:¬ "Characters for tabs and trailing whitespaces
set smartindent "Smarter indentation especially for C files
set noswapfile "Doesn't keep swap files
set wildignore=*.o,*~,*.pyc "Ignore these files (executables)
set wildmenu "Completion for commands
autocmd BufRead,BufNewFile * syn match parens /[\[\](){}]/ | hi parens ctermfg=208
set clipboard=unnamed "Cross-terminal paste

"}}}
" Space & Tabs {{{

set smarttab
set tabstop=4
set shiftwidth=4

"}}}
" Search settings {{{

set incsearch
set ignorecase
set smartcase
set hlsearch
set scrolloff=8
set sidescrolloff=10

" }}}
" Split Layouts{{{

" Specify different areas of the screen
set splitbelow
set splitright
" Split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" }}}
" Misc {{{
"
" Escape delay
set timeoutlen=1000
set ttimeoutlen=10

" Avoid backup files in working directory
set backupdir=~/.vim/tmp,.
set directory=~/.vim/tmp,.

" Persistent undo across sessions
set undofile
set undodir=~/.vim/tmp,.

" }}}
" Mapping {{{

" Treats long lines as break lines
map j gj
map k gk

" Set Space as Leader
let mapleader = "\<Space>"

" Paste mode
:noremap <F2> :set paste! nopaste?<CR>

" Toggles numbers (switch from static to relative)
nnoremap <F3> :NumbersToggle<CR>

" Toggle tabs and trailing spaces display
nmap <silent> <F5> :set list!<CR>

" Toggle NERDTree
map <Leader>l :NERDTreeToggle<CR>

" Open NERDTree
map <Leader>L :NERDTree<CR>

" Copy from cursor to the end of the line
nnoremap Y  y$

" Save with Leader + w
nnoremap <Leader>w :w<CR>

" remote trailing whitespaces
nnoremap <Leader>rtw :%s/\s\+$//e<CR>

" Unbind the cursor keys in insert, normal and visual modes.
for prefix in ['i', 'n', 'v']
	for key in ['<Up>', '<Down>', '<Left>', '<Right>']
		exe prefix . "noremap " . key . " <Nop>"
	endfor
endfor

" Move through buffers with arrow keys
nnoremap <Left> :bprev<CR>
nnoremap <Right> :bnext<CR>

" Move lines up and down with [e and ]e
function! s:Move(cmd, count, map) abort
  normal! m`
  silent! exe 'move'.a:cmd.a:count
  norm! ``
  silent! call repeat#set("\<Plug>unimpairedMove".a:map, a:count)
endfunction

function! s:MoveSelectionUp(count) abort
  normal! m`
  silent! exe "'<,'>move'<--".a:count
  norm! ``
  silent! call repeat#set("\<Plug>unimpairedMoveSelectionUp", a:count)
endfunction

function! s:MoveSelectionDown(count) abort
  normal! m`
  exe "'<,'>move'>+".a:count
  norm! ``
  silent! call repeat#set("\<Plug>unimpairedMoveSelectionDown", a:count)
endfunction

nnoremap <silent> <Plug>unimpairedMoveUp            :<C-U>call <SID>Move('--',v:count1,'Up')<CR>
nnoremap <silent> <Plug>unimpairedMoveDown          :<C-U>call <SID>Move('+',v:count1,'Down')<CR>
noremap  <silent> <Plug>unimpairedMoveSelectionUp   :<C-U>call <SID>MoveSelectionUp(v:count1)<CR>
noremap  <silent> <Plug>unimpairedMoveSelectionDown :<C-U>call <SID>MoveSelectionDown(v:count1)<CR>

nmap [e <Plug>unimpairedMoveUp
nmap ]e <Plug>unimpairedMoveDown
xmap [e <Plug>unimpairedMoveSelectionUp
xmap ]e <Plug>unimpairedMoveSelectionDown

" }}}
