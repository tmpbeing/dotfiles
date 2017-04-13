" **************************************************************************** "
"                                                                              "
"                                                         :::      ::::::::    "
"    init.vim                                           :+:      :+:    :+:    "
"                                                     +:+ +:+         +:+      "
"    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         "
"                                                 +#+#+#+#+#+   +#+            "
"    Created: 2017/02/14 18:26:34 by mplanell          #+#    #+#              "
"    Updated: 2017/04/13 11:18:41 by mplanell         ###   ########.fr        "
"                                                                              "
" **************************************************************************** "

" Load plugins {{{

call plug#begin('~/.config/nvim/plugged')

" Interface
Plug 'ap/vim-buftabline'
Plug 'myusuf3/numbers.vim'
Plug 'itchyny/lightline.vim'
Plug 'Shougo/unite.vim'

" File Browsing
Plug 'Shougo/vimfiler'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'treia/nerdcommenter-42-edition'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" Utilies
Plug 'w0rp/ale'
"Plug 'vim-syntastic/syntastic'
Plug 'terryma/vim-expand-region'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'sickill/vim-pasta'
"Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'euclio/vim-markdown-composer'
Plug 'brooth/far.vim'
Plug 'kassio/neoterm'

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
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Colorscheme / Syntax
Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'

" Misc
Plug 'Treia/42header.vim'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'junegunn/goyo.vim'

call plug#end()

" }}}
" Addons settings {{{

" Set Space as Leader
let mapleader = "\<Space>"

" " Syntastic options
" let g:syntastic_cpp_compiler = 'gcc'
" let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++ -Wall -Werror -Wextra'
" let g:syntastic_check_on_open=1
" let g:syntastic_enable_signs=1
" let g:syntastic_cpp_check_header = 1
" let g:syntastic_cpp_remove_include_errors = 1
" let g:syntastic_c_remove_include_errors = 1
" let g:syntastic_c_include_dirs = ['../../../includes', '../../includes','../includes','./includes''../../../include', '../../include','../include','./include']

" Ale
let g:ale_c_gcc_options = '-Wall -Werror -Wextra'
let g:ale_c_clang_options = '-Wall -Werror -Wextra'

" Nerd commenter
let g:NERDSpaceDelims = 1
let g:NERDTrimTrailingWhitespace = 1

" Nerdtree
let NERDTreeShowHidden=1
let NERDTreeDirArrowExpandable = '▷'
let NERDTreeDirArrowCollapsible = '▼'
" Toggle NerdTree
map <Leader>l :NERDTreeToggle<CR>
" Open NerdTree
map <Leader>L :NERDTree<CR>

" Vimfiler
let g:vimfiler_as_default_explorer = 1
map <Leader>f :VimFiler<CR>

" Far
let g:far#source= 'agnvim'

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
		\   'right': [ [ 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
		\ },
		\'component': {
		\ 'readonly': '%{&readonly?"":""}',
		\ },
		\ 'separator': { 'left': '▓▒░', 'right': '░▒▓' 
		\ },
		\ 'subseparator': { 'left': '▒', 'right': '░' 
		\ }
		\ }

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
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" Find functions with CtrlP
nnoremap <Leader>fu :CtrlPFunky<Cr>

" Vim Markdown Composer
let g:markdown_composer_syntax_theme = "gruvbox-dark"

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/Library/Developer/CommandLineTools/usr/lib/libclang.dylib' 
let g:deoplete#sources#clang#clang_header = '/Library/Developer/CommandLineTools/usr/lib/clang/7.3.0/include'
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif " Autoclose scratch window
" deoplete tab-complete
" inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Neosnippets
" imap <C-k>     <Plug>(neosnippet_expand_or_jump)
" let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

" Ultisnips
let g:UltiSnipsExpandTrigger="<c-s>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Neoterm
let g:neoterm_size=20
let g:neoterm_autoinsert=0
let g:neoterm_keep_term_open=0
nnoremap <silent> <leader>tm :T make<CR>

" Buftabline
let g:buftabline_indicators=1
nmap <D-1> <Plug>BufTabLine.Go(1)
nmap <D-2> <Plug>BufTabLine.Go(2)
nmap <D-3> <Plug>BufTabLine.Go(3)
nmap <D-4> <Plug>BufTabLine.Go(4)
nmap <D-5> <Plug>BufTabLine.Go(5)
nmap <D-6> <Plug>BufTabLine.Go(6)
nmap <D-7> <Plug>BufTabLine.Go(7)
nmap <D-8> <Plug>BufTabLine.Go(8)
nmap <D-9> <Plug>BufTabLine.Go(9)
nmap <D-0> <Plug>BufTabLine.Go(10)

" 42 Header
autocmd FileType make let b:fortytwoheader_delimiters=['#', '#', '*']
nmap <f4> :FortyTwoHeader<CR>

" Toggles numbers (switch from static to relative)
nnoremap <F3> :NumbersToggle<CR>

" }}}
" Colorscheme & Statusline settings {{{

colorscheme gruvbox 
set background=dark
hi CursorLine cterm=bold ctermbg=234
"let g:gruvbox_termcolors = 256
let g:gruvbox_contrast_dark ='medium'
let g:gruvbox_improved_warnings = 1
let g:gruvbox_italic = 1
"set t_Co=256
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

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

" Filetype
filetype on
filetype plugin on
filetype plugin indent on

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

" Open a new buffer separately
nmap <leader>n :enew<cr>

" Detect filetype of current buffer
nmap <leader>ft :filetype detect<cr>

" Saner command-line history
cnoremap <c-n>  <down>
cnoremap <c-p>  <up>

" Redraws screen, clears highlight, refresh syntax highlight
nnoremap <leader>r :nohlsearch<cr>:diffupdate<cr>:syntax sync fromstart<cr><c-l>

" Move line up or down
nnoremap [e  :<c-u>execute 'move -1-'. v:count1<cr>
nnoremap ]e  :<c-u>execute 'move +'. v:count1<cr>

" Open a macro in command line in order to modify it
nnoremap <leader>m  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>

" Don't lose selection when using > or <
xnoremap <  <gv
xnoremap >  >gv

" Toggle tabs and trailing spaces display
nmap <silent> <F5> :set list!<CR>

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

"" <Leader>q Closes the current buffer
nnoremap <silent> <Leader>q :Bclose<CR>

" Unbind Ex Mode
:map Q <Nop>

" }}}
