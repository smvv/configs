" =============================================================================
" Vim config file -- Sander Mathijs van Veen <smvv@kompiler.org> -- since 2011
" =============================================================================

set nocompatible " Be iMproved!

" Terminal configuration
" -----------------------------------------------------------------------------

set background=dark
set t_Co=256
set title           " change the terminal's title
set ttyfast

" Vundle
" -----------------------------------------------------------------------------

"filetype on      " disable OS X exit with non-zero error code
filetype off     " disabled to work around vundle ftdetect bug

"" required for Vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" Utility repos
Bundle 'SirVer/ultisnips'
Bundle 'tpope/vim-fugitive'
Bundle 'wincent/Command-T'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'

" Syntax checking
Bundle 'scrooloose/syntastic'

" Gist integration
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'

" Language repos
Bundle 'wting/rust.vim'

filetype off
filetype plugin indent on

" Disable visual bells
" -----------------------------------------------------------------------------

set visualbell      " don't beep
set noerrorbells    " don't beep
set vb t_vb=        " don't beep

" User inteface options
" -----------------------------------------------------------------------------

set modeline        " Check beginning and end of file for file-specific Vim
                    " settings. Modelines contain the string "vim:" or "ex:".
set autoread        " Read the contents of a file, if it has been modified.
set autowrite       " Write the contents of the file, if it has been modified.
                    " When <C-Z> is pressed, Vim will write to the file.
set showcmd         " Show (partial) command in the last line of the screen.
set showmode        " Show paste mode in the last line of the screen.
set cmdheight=1     " Number of screen lines to use for the command-line.


" Wildmenu
set wildmenu        " use wildmenu
set wildcharm=<TAB> " autocomplete
set wildignore=*.swp,*.bak,*.pyc,*.class,*.old

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
set statusline+=%#warningmsg# " change colors
set statusline+=%{SyntasticStatuslineFlag()} " display syntax errors
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

" NERD Tree
let NERDTreeShowHidden = 1

" UltiSnips
ca use UltiSnipsEdit
let g:UltiSnipsDontReverseSearchPath = "1" " workaround Vundle
let g:UltiSnipsSnippetDirectories = ["UltiSnips", "snippets"]
let g:UltiSnipsEditSplit = "horizontal"
let g:UltiSnipsListSnippets = "<c-l>"
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" Color theme and styling
" -----------------------------------------------------------------------------
if !has('gui_running')
    " Use the guicolorscheme plugin to makes 256-color or 88-color terminal
    " use GUI colors rather than cterm colors
    runtime! plugin/guicolorscheme.vim
    GuiColorScheme darkspectrum
else
    colorscheme darkspectrum
endif

" Highlight current line
au ColorScheme * hi CursorLine term=underline ctermbg=darkblue

" Highlight search color
hi! Search term=reverse ctermfg=255 ctermbg=130

" Make sign column same color as theme
"highlight clear SignColumn
hi! link SignColumn LineNr

" Whitespace control
" -----------------------------------------------------------------------------

"filetype plugin indent on

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

" Show tabs as symbols
set listchars=tab:>\ ,trail:·,extends:⋯,precedes:⋯
set list

" When editing a git commit message (.git/COMMIT_EDITMSG) you often won't start
" on the first line due to Vim remembering your last position in that file
autocmd FileType gitcommit call setpos('.', [0, 1, 1, 0])

" Directories with specific whitespace settings
autocmd BufNewFile,BufRead ~/work/binutils/* set tabstop=8
autocmd BufNewFile,BufRead ~/work/gmake/* set tabstop=8

" Search behaviour
" -----------------------------------------------------------------------------

set ignorecase    " ignore case when searching
set incsearch     " show search matches as you type
set hlsearch      " highlight search terms
set smartcase     " ignore case, if search pattern is all lowercase,
                  " case-sensitive otherwise

" Fix Vim's default key bindings
" -----------------------------------------------------------------------------

" Vertical align the line of next / previous matching search result
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz

" Use Q for formatting the current paragraph or selection
vmap Q gq
nmap Q gqap

" Visual shifting without exiting visual mode
vnoremap < <gv
vnoremap > >gv

" Nice scrolling if line is wrapped
noremap j gj
noremap k gk

" Make Y behave like other capitals
map Y y$

" Split line (opposite to S-J joining line)
nnoremap <silent> <C-J> gEa<CR><ESC>ew

" Use sane regexes
nnoremap / /\v
vnoremap / /\v

" Handy keyboard shortcuts
" -----------------------------------------------------------------------------

" It clears the search buffer (and highlighting)
nmap <silent> <leader>/ :nohlsearch<CR>

" Invoke `sort' command on line selection
nmap <leader>s Vip :!sort<CR>
vmap <silent> <leader>s :!sort<CR>

" Invoke `make' commands
nmap <silent> <F5> :!make<CR>

nnoremap <F8> :NERDTreeToggle<CR>
nnoremap <F9> :call StripTrailingWhitespaces()<CR>
nnoremap <F10> :set invpaste paste?<CR>
set pastetoggle=<F10>

" Show syntax highlighting groups for word under cursor
nmap <F11> :call <SID>DisplaySyntaxGroup()<CR>

" File type corrections for Vim
" -----------------------------------------------------------------------------

" Vim 7.3 does not detect "index ..." lines in filetype=diff
au BufRead,BufNewFile *.patch syn match diffFile "^index .*$"

" Vim 7.3 does not set the proper filetype for TeX files
au BufNewFile,BufRead *.tex set ft=tex
"au BufNewFile,BufRead makefile set ft=make
au BufNewFile,BufRead *.md set ft=markdown

" Miscellaneous utility functions
" -----------------------------------------------------------------------------

function! StripTrailingWhitespaces()
    " prelude
    let _s=@/
    let l = line(".")
    let c = col(".")
    " business time
    %s/\s\+$//e
    " epilogue
    let @/=_s
    call cursor(l, c)
endfunction

"function! <SID>SynStack()
function! <SID>DisplaySyntaxGroup()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
