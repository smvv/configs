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
    alias gnus="emacsclient -c -t -e \(gnus\)"
    alias suspend="dbus-send --system --print-reply \
        --dest=org.freedesktop.UPower /org/freedesktop/UPower \
        org.freedesktop.UPower.Suspend"
fi

# Load system-wide CFLAGS, CXXFLAGS and LDFLAGS.
source /etc/make.conf
export CXXFLAGS="$CFLAGS"

export BROWSER=firefox

export PATH="$PATH:/sbin:/usr/sbin:/usr/libexec"
export HISTCONTROL=ignoreboth:erasedups
export HISTFILESIZE=2000
export LANG=en_US.utf8

. ~/.bash_profile

# Import bash completion files
[[ -f /etc/profile.d/bash-completion.sh ]] \
    && source /etc/profile.d/bash-completion.sh

