" ======================================================================
" Vim configuration file by: Sander Mathijs van Veen <smvv@kompiler.org>
" ======================================================================
set nocompatible    " Disable Vi-compatibility

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

" Set utf-8 encoding as default encoding
set encoding=utf-8
set fileencodings=utf-8,cp1251
set termencoding=utf-8

" My OMGWTFBBQ statusline
set statusline=%f      " path from cwd to filename
set statusline+=\ \    " separator
set statusline+=%h     " help file flag
set statusline+=%m     " modified flag
set statusline+=%r     " read only flag
set statusline+=%=     " left/right separator
set statusline+=%#Comment#  " change colors
set statusline+=%y     " filetype
set statusline+=%0*    " set default colors
set statusline+=\      " separator
set statusline+=%#Constant#  " change colors
set statusline+=%{fugitive#statusline()} " git status line
set statusline+=%0*    " set default colors
set statusline+=\      " separator
set statusline+=%c,    " cursor column
set statusline+=%l/%L  " cursor line/total lines
set statusline+=\ %P   " percent through file

" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
"
" Note: Must allow nesting of autocmds to enable any customizations for quickfix
" buffers.
" Note: Normally, :cwindow jumps to the quickfix window if the command opens it
" (but not if it's already open). However, as part of the autocmd, this doesn't
" seem to happen.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" --------------------
" Disable visual bells
" --------------------

set title           " change the terminal's title
set visualbell      " don't beep
set noerrorbells    " don't beep
set vb t_vb=        " don't beep

" -----------------------
" Color theme and styling
" -----------------------

set background=dark

set t_Co=256
"set t_Co=88
if (&t_Co == 256 || &t_Co == 88)
    if !has('gui_running')
        " Use the guicolorscheme plugin to makes 256-color or 88-color
        " terminal use GUI colors rather than cterm colors.
        runtime! plugin/guicolorscheme.vim
        GuiColorScheme darkspectrum
    else
        colorscheme darkspectrum
    endif
endif

" Vim 7.3 does not detect "index ..." lines in filetype=diff.
au BufRead,BufNewFile *.patch syn match diffFile "^index .*$"

" Automatically reload vimrc after it's modified.
au BufWritePost .vimrc so ~/.vimrc

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
set colorcolumn=80

" Automatically remove all trailing spaces
"autocmd BufWritePre * :%s/\s\+$//e

" Remove all trailing spaces
nnoremap ,W :%s/\s\+$//<cr>:let @/=''<cr>

" show tabs as symbols
set listchars=tab:>\ ,trail:·,extends:⋯,precedes:⋯
set list

" Toggle paste mode
nnoremap <F10> :set invpaste paste?<CR>
set pastetoggle=<F10>
set showmode

" Directories with specific whitespace settings
autocmd BufNewFile,BufRead ~/work/binutils/* set tabstop=8
autocmd BufNewFile,BufRead ~/work/gmake/* set tabstop=8
autocmd BufNewFile,BufRead *.tex set ft=tex

" When editing a git commit message (.git/COMMIT_EDITMSG) you often won't start
" on the first line due to Vim remembering your last position in that file.
autocmd FileType gitcommit call setpos('.', [0, 1, 1, 0])

" ----------------
" Search behaviour
" ----------------

set ignorecase    " ignore case when searching
set incsearch     " show search matches as you type
set hlsearch      " highlight search terms
set smartcase     " ignore case, if search pattern is all lowercase,
                  " case-sensitive otherwise

nnoremap ,n nzz
nnoremap ,N Nzz

" ---------------
" Version control
" ---------------

" Git fugitive menu
menu G.Status :Gstatus<CR>
menu G.Diff :Gdiff<CR>
menu G.Commit :Gcommit %<CR>
menu G.Checkout :Gread<CR>
menu G.Remove :Gremove<CR>
menu G.Move :Gmove<CR>
menu G.Log :Glog<CR>
menu G.Blame :Gblame<CR>

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

" Make Y behave like other capitals.
map Y y$

" It clears the search buffer (and highlighting) when you press ",/".
nmap <silent> ,/ :nohlsearch<CR>

" Invoke `sort' command on line selection when you press ",s".
vmap <silent> ,s :!sort<CR>

" Run current buffer in O'Caml interpreter when you press ",o".
nmap <silent> ,o :!ocaml %<CR>

" Invoke `pyflakes' command when you press ",py".
nmap <silent> ,py :!pyflakes .<CR>

" Invoke `make' commands when you press ",mX".
nmap <silent> ,ma :!make<CR>
nmap <silent> ,mt :!make test<CR>
nmap <silent> ,mc :!make coverage<CR>
nmap <silent> ,mh :!make html<CR>
nmap <silent> ,mm :make<CR><cr>

" Open fugitive menu
map <F9> :emenu G.<TAB>

" Show syntax highlighting groups for word under cursor
nmap <F11> :call <SID>SynStack()<CR>
function! <SID>SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
