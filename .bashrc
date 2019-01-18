# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
alias ..='cd ..'
alias ~='cd ~'
alias ll='ls -al'
function cd() { builtin cd "$@"; ls; }
function cdd() { builtin cd "$@"; }
