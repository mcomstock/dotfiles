set encoding=utf8
scriptencoding=utf8

filetype plugin on
syntax on
colorscheme color_to_the_max

set termguicolors

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

set backspace=indent,eol,start

if has('nvim')
	call plug#begin('~/.local/share/nvim/plugged')

	Plug 'sheerun/vim-polyglot'
	Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'
	Plug 'vim-airline/vim-airline'

	call plug#end()

	let g:polyglot_disabled = ['markdown']

	let g:airline_powerline_fonts = 1
endif
