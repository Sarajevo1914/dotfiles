#
# ~/.bashrc
#

### Setings

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# disable ctrl-s and ctrl-q.
stty -ixon 

# allow you to cd into directory merely by tiping the directory name
shopt -s autocd 

# autocorrects cd misspellings
shopt -s cdspell 

# expand aliases
shopt -s expand_aliases 

# ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"


### History in cache directory

#HISTTIMEFORMAT="%F %T "

HISTSIZE=10000000
SAVEHIST=10000000


### SET MANPAGER

### "bat" as manpager
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

### "vim" as manpager
#export MANPAGER='/bin/bash -c "vim -MRn -c \"set buftype=nofile showtabline=0 ft=man ts=8 nomod nolist norelativenumber nonu noma\" -c \"normal L\" -c \"nmap q :qa<CR>\"</dev/tty <(col -b)"'

### "nvim" as manpager
# export MANPAGER="nvim -c 'set ft=man' -"

### "less" as manpager
#export MANPAGER="less"


### Prompt

PS1='[\u@\h \W]\$ '


### Alias

# Verbosity and settings that you pretty much just always are going to want.
alias \
	cp="cp -iv"	\
	..="cd .."	\
	mv="mv -iv" \
	rm="rm -vI" \
	bc="bc -ql" \
	ls="lsd -A --group-dirs first"	\
	ll="lsd -lA --group-dirs first --total-size"	\
	llt="lsd -lAt --group-dirs first --total-size"	\
	lls="lsd -lAS --group-dirs first --total-size"	\
	rsync="rsync -vrPlu" \
	mkd="mkdir -pv" \
	yt="yt-dlp --embed-metadata -i" \
	yta="yt -x -f bestaudio/best" \
	ytt="yt --skip-download --write-thumbnail" \
	ffmpeg="ffmpeg -hide_banner"


# These common commands are just too long! Abbreviate them.
alias \
	ns="nsxiv-rifle"


# Colorize commands when possible.
alias \
	grep="grep --color=auto"	\
	diff="diff --color=auto"	\
	ccat="highlight --out-format=ansi"	\
	ip="ip -color=auto"	\
	ls="ls --color=auto"
