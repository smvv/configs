set ruler
set expandtab
set modeline

set tabstop=4     " a tab is four spaces
set backspace=indent,eol,start
                  " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
"set number        " always show line numbers
set shiftwidth=4  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                  "    case-sensitive otherwise
set smarttab      " insert tabs on the start of a line according to
                  "    shiftwidth, not tabstop
set hlsearch      " highlight search terms
set incsearch     " show search matches as you type

set history=1000    " remember more commands and search history
set undolevels=1000 " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class,*.old

set textwidth=79

" IMPORTANT: Uncomment one of the following lines to force
" using 256 colors (or 88 colors) if your terminal supports it,
" but does not automatically use 256 colors by default.
set background=dark

" -- run in gnome-terminal: --
" export TERM="xterm-256color"
set t_Co=256

augroup vimrc_autocmds
    autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929
    autocmd BufEnter * match OverLength /\%81v.*/
augroup END

"" automatically open and close the popup menu / preview window
"au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
""set completeopt=menuone,menu,longest,preview
"set completeopt=menuone,menu " ,longest,preview

"set nocp
"filetype plugin on

colorscheme darkspectrum

" ------------------
" Various vim tweaks
" ------------------
set title           " change the terminal's title
set visualbell      " don't beep
set noerrorbells    " don't beep
set vb t_vb=        " don't beep
set nocp            " Disable Vi-compatibility

" syntax detection and filetype indention
filetype plugin indent on
syntax enable

" autocmd BufReadPre *.pdf set ro nowrap
" autocmd BufReadPost *.pdf silent %!pdftotext "%" -nopgbrk -layout -q -eol unix -

" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" -----------------
" Source completion
" -----------------
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
"set completeopt=menuone,menu

" --------------
" Source folding
" --------------
"set foldmethod=indent

" -------------
" File explorer
" -------------

let g:netrw_liststyle=3    " Use tree-mode as default view
"let g:netrw_browse_split=4 " Open file in previous buffer
let g:netrw_preview=1      " Preview window shown in a vertically split

" ------------
" Key bindings
" ------------

" skip the press and hold 'shift' + press ';' + release 'shift'.
" Now you can simply use ';' instead of ':'.
nnoremap ; :

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

" If you like long lines with line wrapping enabled, this solves the problem
" that pressing down jumpes your cursor “over” the current line to the next
" line. It changes behaviour so that it jumps to the next row in the editor.
nnoremap j gj
nnoremap k gk

" Easy window navigation
"map <C-h> <C-w>h
"map <C-j> <C-w>j
"map <C-k> <C-w>k
"map <C-l> <C-w>l

" It clears the search buffer (and highlighting) when you press ,/
nmap <silent> ,/ :nohlsearch<CR>

" Invoke `make' command when you press ",m".
nmap <silent> ,m :!make<CR>

" when you forgot to sudo before editing a file that requires root privileges.
" This lets you use w!! to do that after you opened the file already:
cmap w!! w !sudo tee % >/dev/null
