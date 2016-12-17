syntax on
filetype plugin indent on
filetype on
filetype plugin on

"
"
" Addons settings
"
"

execute pathogen#infect()
execute pathogen#helptags()
let g:airline_powerline_fonts = 1

" Syntastic options
let g:syntastic_cpp_compiler = 'gcc'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++ -Wall -Werror -Wextra'
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_remove_include_errors = 1
let g:syntastic_c_remove_include_errors = 1
let g:syntastic_c_include_dirs = ['../../../include', '../../include','../include','./include','./includes','../includes','../../includes','../../../includes']

" Nerd commenter
let g:NERDSpaceDelims = 1
let g:NERDTrimTrailingWhitespace = 1

" Select expanding regions through spamming v
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)
let g:expand_region_text_objects = {
		\ 'i"'  :1,
		\ 'i''' :1,
		\ 'ip'  :1,
		\ 'iB'  :1,
		\ 'ib'  :1,
		\ 'i['  :1,
		\ }
call expand_region#custom_text_objects({
	\ "\/\\n\\n\<CR>" : 1,
	\ })

" Theme settings
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-chalk
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

set laststatus=2

"
"
" General stuff
"
"

set autoindent
set cursorline
set noswapfile
set wildignore=*.o,*~,*.pyc
set showmatch
set backspace=indent,eol,start
set nocompatible
set number
set cc=80
set whichwrap+=<,>,h,l,[,]

" Tab settings
set smarttab
set tabstop=4
set shiftwidth=4
"set list listchars=tab:»·,trail:·
set list listchars=tab:▸\ ,trail:·

" Search settings
set ignorecase
set smartcase
set hlsearch
set scrolloff=8
set sidescrolloff=10

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
set clipboard=unnamedplus
:noremap <F2> :set paste! nopaste?<CR>

nnoremap <F3> :NumbersToggle<CR>


let mapleader = "\<Space>"
nnoremap <Leader>w :w<CR>

" Unbind the cursor keys in insert, normal and visual modes.
for prefix in ['i', 'n', 'v']
	for key in ['<Up>', '<Down>', '<Left>', '<Right>']
		exe prefix . "noremap " . key . " <Nop>"
	endfor
endfor
