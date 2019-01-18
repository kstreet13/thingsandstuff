# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:bin/msys-ssh-1000-18:$PATH

export PATH

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
export PS1="\e[1;34m[\u:\W/]\$ \e[m"
