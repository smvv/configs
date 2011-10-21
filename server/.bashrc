# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

alias emacs="emacsclient -c -t"
alias bugmoz="bugz --connection Mozilla"
alias vim="gvim -v"

export TERM=xterm-256color
export PATH="/usr/lib64/ccache/bin:$PATH"
export TIMEFORMAT="elapsed: %3lR (usr %3lU; sys %3lS; cpu %P%%)"

alias mf="make -ffast.mk"

function rgrep {
    grep $1 -Rn .
}

function extract()
{
   if [ -f $1 ] ; then
       case $1 in
           *.tar.bz2)   tar xvjf $1     ;;
           *.tar.gz)    tar xvzf $1     ;;
           *.bz2)       bunzip2 $1      ;;
           *.rar)       unrar x $1      ;;
           *.gz)        gunzip $1       ;;
           *.tar)       tar xvf $1      ;;
           *.tbz2)      tar xvjf $1     ;;
           *.tgz)       tar xvzf $1     ;;
           *.zip)       unzip $1        ;;
           *.Z)         uncompress $1   ;;
           *.7z)        7z x $1         ;;
           *)           echo "'$1' cannot be extracted via >extract<" ;;
       esac
   else
       echo "'$1' is not a valid file"
   fi
}

alias s="pwd > ~/.saved_dir"
alias i="cd \"\$(cat ~/.saved_dir)\""
