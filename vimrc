set encoding=utf8
scriptencoding utf8

filetype plugin on
syntax on

" Relative line numbering
set number relativenumber

" Don't close current buffer when opening a new one
set hidden

set linebreak
set showmatch
set visualbell
set ruler
set background=dark

set hlsearch
set ignorecase
set smartcase
set incsearch

set autoindent
set shiftwidth=4
set smartindent
set smarttab
set tabstop=4
set softtabstop=4
set virtualedit=onemore

set backspace=indent,eol,start

set nobackup noswapfile

set listchars=eol:$,tab:»\ ,trail:\ ,space:·,precedes:←,extends:→

" Custom functions
function! RemoveTrailingSpaces()
	let l:l = line('.')
	let l:c = col('.')

	%s/\s\+$//e

	call cursor(l:l, l:c)
endfunction

" Custom commands
let g:mapleader=' '
let g:maplocalleader=g:mapleader

command! -bar -nargs=0 RemoveTrailingSpaces call RemoveTrailingSpaces()

map <silent> <leader><leader> :nohlsearch<cr>
map <silent> <leader>tw :set list!<cr>
map <silent> <leader>rw :RemoveTrailingSpaces<cr>

if has('nvim')
	call plug#begin('~/.local/share/nvim/plugged')

	Plug 'sheerun/vim-polyglot'
	Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'
	Plug 'vim-airline/vim-airline'

	call plug#end()

	let g:polyglot_disabled = ['markdown']

	map <leader>ff :Files<cr>

	let g:airline_powerline_fonts = 1
endif

if has('termguicolors')
	set termguicolors
	colorscheme color_to_the_max

	" Highlight trailing whitespace outside insert mode.
	hi ExtraWhitespace guifg=#ff0000 guibg=#ff0000
	match ExtraWhitespace /\s\+$/
	augroup whitespace
		autocmd!
		autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
		autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
		autocmd InsertLeave * match ExtraWhitespace /\s\+$/
		autocmd BufWinLeave * call clearmatches()
	augroup END
else
	colorscheme default
endif
