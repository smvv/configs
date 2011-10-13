set autoindent    " always set autoindenting on
set backspace=indent,eol,start
set copyindent    " copy the previous indentation on autoindenting
set expandtab
set history=1000    " remember more commands and search history
set hlsearch      " highlight search terms
set ignorecase    " ignore case when searching
set incsearch     " show search matches as you type
set modeline
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set shiftwidth=4
set scrolloff=2   " 2 lines above/below cursor when scrolling
set smartcase     " ignore case if search pattern is all lowercase,case-sensitive otherwise
set smarttab      " insert tabs on the start of a line according to shiftwidth, not tabstop
set tabstop=4
set textwidth=79
set undolevels=1000 " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class,*.old

filetype indent plugin on

set background=dark

set t_Co=256
"set t_Co=88
if (&t_Co == 256 || &t_Co == 88) && !has('gui_running') &&
  \ filereadable(expand("$HOME/.vim/plugin/guicolorscheme.vim"))
    " Use the guicolorscheme plugin to makes 256-color or 88-color
    " terminal use GUI colors rather than cterm colors.
    runtime! plugin/guicolorscheme.vim
    GuiColorScheme darkspectrum
endif

" ------------------
" Various vim tweaks
" ------------------
set title           " change the terminal's title
set visualbell      " don't beep
set noerrorbells    " don't beep
set vb t_vb=        " don't beep
set nocp            " Disable Vi-compatibility

" Automatically remove all trailing spaces
"autocmd BufWritePre * :%s/\s\+$//e

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

nmap ,s Vip :!sort<CR> 

" It clears the search buffer (and highlighting) when you press ,/
nmap <silent> ,/ :nohlsearch<CR>

" Invoke `sort' command on line selection when you press ",s".
vmap <silent> ,s :!sort<CR>

" Invoke `pyflakes' command when you press ",p".
nmap <silent> ,p :!pyflakes .<CR>

" Invoke `make' command when you press ",m".
nmap <silent> ,m :!make<CR>
