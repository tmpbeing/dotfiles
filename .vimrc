runtime! archlinux.vim
syntax on
filetype plugin indent on
filetype on
filetype plugin on

" Addons settings
execute pathogen#infect()
execute pathogen#helptags()
let g:airline_powerline_fonts = 1

" Promptline setup
let g:promptline_preset = {
        \'a'    : [ '$USER' ],
        \'b'    : [ promptline#slices#cwd() ],
        \'c'    : [ promptline#slices#vcs_branch() ],
        \'warn' : [ promptline#slices#last_exit_code() ]}

" Syntastic options
let g:syntastic_cpp_compiler = 'gcc'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++ -Wall -Werror -Wextra'
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_remove_include_errors = 1
let g:syntastic_c_remove_include_errors = 1
let g:syntastic_c_include_dirs = ['../../../include', '../../include','../include','./include']

" Theme settings
let g:gruvbox_contrast_dark='neutral'
colorscheme gruvbox
set background=dark

set laststatus=2

" General stuff
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
set list listchars=tab:»·,trail:·

" Search settings
set ignorecase
set smartcase
set hlsearch
set scrolloff=8
set sidescrolloff=10

" Escape delay
set timeoutlen=1000
set ttimeoutlen=10

" Cross-terminal paste
set clipboard=unnamed
:noremap <F2> :set paste! nopaste?<CR>

nnoremap <F3> :NumbersToggle<CR>


" Unbind the cursor keys in insert, normal and visual modes.
for prefix in ['i', 'n', 'v']
  for key in ['<Up>', '<Down>', '<Left>', '<Right>']
    exe prefix . "noremap " . key . " <Nop>"
  endfor
endfor
