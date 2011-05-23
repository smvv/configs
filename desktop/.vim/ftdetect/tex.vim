set errorformat=%f:%l:%c:%m
function Latex()
    update
    let file=expand('%:t:r')
    let opts='-src -shell-escape -interaction=nonstopmode'
	let errors=system('pdflatex '.opts.' '.file.'|~/.vim/script/latex-errorfilter')
    if errors==""
        echo 'LaTeX ok: No warning/error'
    else
        cexpr errors
    endif
endfunction

map <F4> :call Latex()<CR>
map <F6> :cprev<CR>
map <F7> :cnext<CR>
map <F8> :clist<CR> 
