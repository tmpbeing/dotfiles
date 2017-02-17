" **************************************************************************** "
"                                                                              "
"                                                         :::      ::::::::    "
"    init.vim                                           :+:      :+:    :+:    "
"                                                     +:+ +:+         +:+      "
"    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         "
"                                                 +#+#+#+#+#+   +#+            "
"    Created: 2017/02/14 18:26:34 by mplanell          #+#    #+#              "
"    Updated: 2017/02/17 02:31:39 by mplanell         ###   ########.fr        "
"                                                                              "
" **************************************************************************** "

" Load plugins {{{

call plug#begin('~/.config/nvim/plugged')

" Interface
Plug 'ap/vim-buftabline'
Plug 'myusuf3/numbers.vim'
Plug 'itchyny/lightline.vim'

" File Browsing
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'treia/nerdcommenter-42-edition'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" Utilies
Plug 'vim-syntastic/syntastic'
Plug 'terryma/vim-expand-region'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'sickill/vim-pasta'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Completion/Snippets
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"Plug 'Shougo/neosnippet'
"Plug 'Shougo/neosnippet-snippets'
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-zsh'
Plug 'Shougo/neco-vim'
Plug 'Shougo/neco-syntax'
Plug 'Shougo/neoinclude.vim'

" Colorscheme
Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'

" Misc
Plug 'pandark/42header.vim'

call plug#end()

" }}}
" Addons settings {{{
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

" Nerdtree
let NERDTreeShowHidden=1
let NERDTreeDirArrowExpandable = '▷'
let NERDTreeDirArrowCollapsible = '▼'

" " Promptline
" let g:promptline_preset = {
		" \'a'    : [ '$USER' ],
		" \'b'    : [ promptline#slices#cwd() ],
		" \'c'    : [ promptline#slices#vcs_branch() ],
		" \'warn' : [ promptline#slices#last_exit_code() ]}
" let g:promptline_symbols = {
		" \'left' : '',
		" \'dir_sep' : '',}

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

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/include/clang'

" Neosnippets
" imap <C-k>     <Plug>(neosnippet_expand_or_jump)
" let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

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

" }}}
" UI Config {{{

set cursorline "Highlight current line
set showmatch "Shows matching braces
set number "show line numbers
set hidden "hides new buffers if necessary
set cc=80 "Highlights column 80
set whichwrap+=<,>,h,l,[,]
set foldmethod=marker
set list listchars=tab:▸\ ,trail:·,nbsp:¬ "Characters for tabs and trailing whitespaces
set smartindent "Smarter indentation especially for C files
set noswapfile "Doesn't keep swap files
set wildignore=*.o,*~,*.pyc "Ignore these files (executables)
autocmd BufRead,BufNewFile * syn match parens /[\[\](){}]/ | hi parens ctermfg=208
set clipboard=unnamed "Cross-terminal paste
set icm=nosplit
set so=7 "set 7 lines to the cursors when moving vertical

" }}}
" Space & Tabs {{{

set tabstop=4
set shiftwidth=4

"}}}
" Search settings {{{

set ignorecase
set smartcase
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

" Escape delay
set timeoutlen=1000
set ttimeoutlen=10

" }}}
" Mapping {{{

" Treats long lines as break lines
map j gj
map k gk

" Set Space as Leader
let mapleader = "\<Space>"

" Toggles numbers (switch from static to relative)
nnoremap <F3> :NumbersToggle<CR>

" 42 School Header
nmap <f4> :Fortytwoheader<CR>

" Find functions with CtrlP
nnoremap <Leader>fu :CtrlPFunky<Cr>

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

" Search for word under the cursor
nnoremap <leader>/ "fyiw :/<c-r>f<cr>

" Scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Unbind the cursor keys in insert, normal and visual modes.
for prefix in ['i', 'n', 'v']
	for key in ['<Up>', '<Down>', '<Left>', '<Right>']
		exe prefix . "noremap " . key . " <Nop>"
	endfor
endfor

" Move through buffers with arrow keys
nnoremap <Left> :bprev!<CR>
nnoremap <Right> :bnext!<CR>

" Unbind Ex Mode
:map Q <Nop>

" }}}
