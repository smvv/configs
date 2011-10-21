" ======================================================================
" Vim configuration file by: Sander Mathijs van Veen <smvv@kompiler.org>
" ======================================================================

" ---------------------
" User inteface options
" ---------------------

set modeline        " Check beginning and end of file for file-specific Vim
                    " settings. Modelines contain the string "vim:" or "ex:".
set autoread        " Read the contents of a file, if it has been modified.
set autowrite       " Write the contents of the file, if it has been modified.
                    " When <C-Z> is pressed, Vim will write to the file.
set showcmd         " Show (partial) command in the last line of the screen.
set cmdheight=1     " Number of screen lines to use for the command-line.


" Wildmenu
set wildmenu        " use wildmenu
set wildcharm=<TAB> " autocomplete
set wildignore=*.swp,*.bak,*.pyc,*.class,*.old


" Open help in a vsplit rather than a split
command! -nargs=? -complete=help Help :vertical help <args>
cabbrev h h<C-\>esubstitute(getcmdline(), '^h\>', 'Help', '')<CR>

" Enable persistent undo history
set undofile
set undodir=~/.vim/undo
set history=1000    " remember more commands and search history
set undolevels=1000 " use many muchos levels of undo

" --------------------
" Disable visual bells
" --------------------

set title           " change the terminal's title
set visualbell      " don't beep
set noerrorbells    " don't beep
set vb t_vb=        " don't beep
set nocompatible    " Disable Vi-compatibility

" -----------------------
" Color theme and styling
" -----------------------

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
" Whitespace control
" ------------------

filetype indent plugin on

set backspace=indent,eol,start
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set expandtab
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set shiftwidth=4
set scrolloff=2   " 2 lines above/below cursor when scrolling
set smarttab      " insert tabs on the start of a line according to
                  " shiftwidth, not tabstop
set tabstop=4
set textwidth=79

" Automatically remove all trailing spaces
"autocmd BufWritePre * :%s/\s\+$//e

" show tabs as symbols
set listchars=tab:>-
set list

" ----------------
" Search behaviour
" ----------------

set ignorecase    " ignore case when searching
set incsearch     " show search matches as you type
set hlsearch      " highlight search terms
set smartcase     " ignore case, if search pattern is all lowercase,
                  " case-sensitive otherwise

" ---------------
" Handy shortcuts
" ---------------

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

" visual shifting without exiting visual mode!
vnoremap < <gv
vnoremap > >gv


" Nice scrolling if line is wrapped.
noremap j gj
noremap k gk


" Quickfix fast navigation
nnoremap <silent> ,nn :cwindow<CR>:cn<CR>
nnoremap <silent> ,pp :cwindow<CR>:cp<CR>

nmap ,s Vip :!sort<CR> 

" It clears the search buffer (and highlighting) when you press ",/".
nmap <silent> ,/ :nohlsearch<CR>

" Invoke `sort' command on line selection when you press ",s".
vmap <silent> ,s :!sort<CR>

" Invoke `pyflakes' command when you press ",py".
nmap <silent> ,py :!pyflakes .<CR>

" Invoke `make' commands when you press ",mX".
nmap <silent> ,ma :!make<CR>
nmap <silent> ,mt :!make test<CR>
