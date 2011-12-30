# /etc/skel/.bashrc
#
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

# Put your fun stuff here.
if [[ "$TERM" != "linux" ]] ; then
    export TERM=xterm-256color

    # Command aliases
    alias vim="gvim -v"
    alias emacs="emacsclient -c -t"
    alias suspend="dbus-send --system --print-reply \
        --dest=org.freedesktop.UPower /org/freedesktop/UPower \
        org.freedesktop.UPower.Suspend"
fi

alias fx="~/moz/fx/obj-i686-pc-linux-gnu/dist/bin/firefox \
    -profile ~/.mozilla/firefox/pumlekcd.default &"

alias vo20="autossh -M 20001 -tt vo20 'screen -UxaR'"
alias openov="autossh -M 20003 -tt openov 'screen -UxaR'"
alias multivac="autossh -M 20005 -tt multivac 'screen -UxaR -S multivac'"
alias multicom="autossh -M 20007 -tt multivac 'screen -UxaR -S communication'"

alias rt="killall radiotray; radiotray &>/dev/null &"
alias sabnzbd="/usr/src/sabnzbd/SABnzbd.py -b 0 &>/dev/null &"
alias x="startx & vlock"

# Load system-wide CFLAGS, CXXFLAGS and LDFLAGS.
source /etc/make.conf
export CXXFLAGS="$CFLAGS"

export BROWSER=firefox
export EDITOR="/usr/bin/gvim -v"

export PATH="/usr/lib64/ccache/bin:$PATH"
export PATH="$PATH:/sbin:/usr/sbin:/usr/libexec"

export HISTCONTROL=ignoreboth:erasedups
export HISTFILESIZE=2000
export LANG=en_US.utf8

export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \[\033[00m\]\$ '

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

. ~/.bash_profile

alias s="pwd > ~/.saved_dir"
alias i="cd \"\$(cat ~/.saved_dir)\""
#alias dvd="mplayer dvdnav://1 -dvd-device"
alias win7="qemu-kvm -m 800 -hda ~/vm/win7.img &"

function dvd()
{
    f=$(realpath "$1")
    mplayer dvdnav://1 -dvd-device "$f"
}

# Import bash completion files
[[ -f /etc/profile.d/bash-completion.sh ]] \
    && source /etc/profile.d/bash-completion.sh

