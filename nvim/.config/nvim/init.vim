" **************************************************************************** "
"                                                                              "
"                                                         :::      ::::::::    "
"    init.vim                                           :+:      :+:    :+:    "
"                                                     +:+ +:+         +:+      "
"    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         "
"                                                 +#+#+#+#+#+   +#+            "
"    Created: 2017/02/14 18:26:34 by mplanell          #+#    #+#              "
"    Updated: 2019/02/03 11:03:50 by mplanell         ###   ########.fr        "
"                                                                              "
" **************************************************************************** "

" Load plugins {{{

call plug#begin('~/.config/nvim/plugged')

" Interface / Display
Plug 'ap/vim-buftabline'
Plug 'myusuf3/numbers.vim'
Plug 'Shougo/unite.vim'
Plug 'mhinz/vim-startify'

" File Browsing
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'jlanzarotta/bufexplorer'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'majutsushi/tagbar'

" Utilies
" Plug 'w0rp/ale'
Plug 'vim-syntastic/syntastic'
Plug 'terryma/vim-expand-region'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'jtratner/vim-flavored-markdown', { 'for': 'markdown' }
Plug 'brooth/far.vim'
Plug 'treia/nerdcommenter-42-edition'
Plug 'godlygeek/tabular'

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv'

" Completion/Snippets
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-clang'
Plug 'Shougo/neco-vim'
Plug 'Shougo/neco-syntax'
Plug 'Shougo/neoinclude.vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Colorscheme
Plug 'morhetz/gruvbox'
Plug 'ayu-theme/ayu-vim'

" Misc
Plug 'Treia/42header.vim'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'vimwiki/vimwiki'

call plug#end()

" }}}
" Addons settings {{{

" Set Space as Leader
let mapleader = "\<Space>"

" " Syntastic
let g:syntastic_cpp_compiler = 'gcc'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++ -Wall -Werror -Wextra'
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_remove_include_errors = 1
let g:syntastic_c_remove_include_errors = 1
let g:syntastic_c_include_dirs = ['../../../includes', '../../includes','../includes','./includes''../../../include', '../../include','../include','./include', './libft/includes', '../libft/includes', '../../libft/includes', '../../../libft/includes', './libft/include', '../libft/include', '../../libft/include', '../../../libft/include']

" Ale
" let g:ale_c_gcc_options = '-Wall -Werror -Wextra'
" let g:ale_c_clang_options = '-Wall -Werror -Wextra'
" let g:ale_lint_on_text_changed = 'never'

" Nerd commenter
let g:NERDSpaceDelims = 1
let g:NERDTrimTrailingWhitespace = 1

" Nerdtree
let NERDTreeShowHidden=1
let NERDTreeDirArrowExpandable = '▷'
let NERDTreeDirArrowCollapsible = '▼'

" Toggle NerdTree
map <Leader>t :NERDTreeToggle<CR>
" open NerdTree
map <Leader>T :NERDTree<CR>

" Far
let g:far#source= 'vimgrep'

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

" FZF
nnoremap <c-p> :Files<CR>
nnoremap <Leader>p :Files ~/<CR>

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/include/clang'
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif " Autoclose scratch window

" Tagbar
nmap <F8> :TagbarToggle<CR>

" Ultisnips
let g:UltiSnipsExpandTrigger="<c-s>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Neoterm
let g:neoterm_size=20
let g:neoterm_autoinsert=5
let g:neoterm_keep_term_open=0
nnoremap <silent> <leader>tm :T make<CR>

" BufTabLine
let g:buftabline_indicators=1
" let g:buftabline_numbers=2
nmap <leader>1 <Plug>BufTabLine.Go(1)
nmap <leader>2 <Plug>BufTabLine.Go(2)
nmap <leader>3 <Plug>BufTabLine.Go(3)
nmap <leader>4 <Plug>BufTabLine.Go(4)
nmap <leader>5 <Plug>BufTabLine.Go(5)
nmap <leader>6 <Plug>BufTabLine.Go(6)
nmap <leader>7 <Plug>BufTabLine.Go(7)
nmap <leader>8 <Plug>BufTabLine.Go(8)
nmap <leader>9 <Plug>BufTabLine.Go(9)
nmap <leader>0 <Plug>BufTabLine.Go(10)

" 42 Header
autocmd FileType make let b:fortytwoheader_delimiters=['#', '#', '*']
nmap <f4> :FortyTwoHeader<CR>

" Toggles numbers (switch from static to relative)
nnoremap <F3> :NumbersToggle<CR>

" vimwiki/vimwiki
let g:vimwiki_list = [{'path': '~/Stuff/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]

" Startify
let g:startify_list_order = [
	\ ['   Files:'], 'files',
	\ ['   This Directory:'], 'dir',
	\ ['   Sessions:'], 'sessions',
	\ ['   Bookmarks:'], 'bookmarks',
	\ ['   Commands:'], 'commands',
	\ ]
let g:startify_custom_header = [
	\ '                    (_)        .       .                                     .',
	\ '     .        ____.--^.',
	\ '      .      /:  /    |                               +           .         .',
	\ "            /:  `--=--'   .                                                .",
	\ '           /: __[\==`-.___          *           .',
	\ '          /__|\ _~~~~~~   ~~--..__            .             .',
	\ '          \   \|::::|-----.....___|~--.                                 .',
	\ '           \ _\_~~~~~-----:|:::______//---...___',
	\ '       .   [\  \  __  --     \       ~  \_      ~~~===------==-...____',
	\ '           [============================================================-',
	\ "           /         __/__   --  /__    --       /____....----''''~~~~      .",
	\ "     *    /  /   ==           ____....=---='''~~~~ .",
	\ "         /____....--=-''':~~~~                      .                .",
	\ '                        .                                   .           .',
	\ '                             .                      .             +',
	\ '           .     +              .'
	\ ]
" }}}
" Colorscheme & Statusline settings {{{

colorscheme ayu
let ayucolor="dark"
set termguicolors
" set background=dark
" let g:gruvbox_contrast_dark ='medium'
" let g:gruvbox_improved_warnings = 1
" let g:gruvbox_italic = 1
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
hi VertSplit ctermfg=238 ctermbg=235
hi StatusLine ctermfg=235 ctermbg=245
hi StatusLineNC ctermfg=235 ctermbg=237
hi CursorLine cterm=bold ctermbg=234
hi clear SignColumn
hi SignColumn ctermbg=235
hi GitGutterAdd ctermbg=235 ctermfg=245
hi GitGutterChange ctermbg=235 ctermfg=245
hi GitGutterDelete ctermbg=235 ctermfg=245
hi GitGutterChangeDelete ctermbg=235 ctermfg=245
hi SyntasticErrorSign ctermfg=9 ctermbg=235
hi SyntasticWarningSign ctermfg=11 ctermbg=235
" hi ALEErrorSign ctermfg=9 ctermbg=235
" hi ALEWarningSign ctermfg=11 ctermbg=235
hi EndOfBuffer ctermfg=237 ctermbg=235
hi BufTabLineActive ctermfg=245 ctermbg=235
hi BufTabLineCurrent ctermfg=245 ctermbg=235
hi BufTabLineHidden ctermfg=239 ctermbg=235
hi ColorColumn ctermbg=236

set statusline=%=%P\ %f\ %m
set showmode

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
set wildignore+=*.o,*~,*.pyc "Ignore these files (executables)
autocmd BufRead,BufNewFile * syn match parens /[\[\](){}]/ | hi parens ctermfg=208
set clipboard=unnamedplus "Cross-terminal paste
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

inoremap jk <Esc>

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

" Save as sudo with w!!
cmap w!! w !sudo tee > /dev/null %

" Upper/lower word
nmap <leader>u mQviwU`Q
nmap <leader>l mQviwu`Q

" Upper/lower first char of word
nmap <leader>U mQgewvU`Q
nmap <leader>L mQgewvu`Q

" }}}
