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
		\ 'colorscheme': 'Mashup',
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
		\ 'separator': { 'left': '▓▒░', 'right': '░▒▓' },
		\ 'subseparator': { 'left': '▒', 'right': '░' }
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

" }}}

" Colorscheme & Statusline settings {{{

colorscheme treia
set background=dark
hi CursorLine cterm=bold ctermbg=234
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
"set nofoldenable "No folders cause they suck
"set list listchars=tab:»·,trail:·
set list listchars=tab:▸\ ,trail:·,nbsp:¬ "Characters for tabs and trailing whitespaces
set smartindent "Smarter indentation especially for C files
set noswapfile "Doesn't keep swap files
set wildignore=*.o,*~,*.pyc "Ignore these files (executables)
set wildmenu "Completion for commands
autocmd BufRead,BufNewFile * syn match parens /[\[\](){}]/ | hi parens ctermfg=208

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

""specify different areas of the screen
set splitbelow
set splitright
""split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"}}}

" Escape delay
set timeoutlen=1000
set ttimeoutlen=10

" Treats long lines as break lines
map j gj
map k gk

" Avoid backup files in working directory
set backupdir=~/.vim/tmp,.
set directory=~/.vim/tmp,.

" Persistent undo across sessions
set undofile
set undodir=~/.vim/tmp,.

" Cross-terminal paste
set clipboard=unnamed

"
"
" Mapping
"
"

" Set Space as Leader
let mapleader = "\<Space>"
" Paste mode
:noremap <F2> :set paste! nopaste?<CR>

" Toggles numbers (switch from static to relative)
nnoremap <F3> :NumbersToggle<CR>

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
