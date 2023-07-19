#
# ~/.bashrc
#


# Setings

PS1='[\u@\h \W]\$ '

### SET MANPAGER

### "bat" as manpager
#export MANPAGER="sh -c 'col -bx | bat -l man -p'"

### "vim" as manpager
#export MANPAGER='/bin/bash -c "vim -MRn -c \"set buftype=nofile showtabline=0 ft=man ts=8 nomod nolist norelativenumber nonu noma\" -c \"normal L\" -c \"nmap q :qa<CR>\"</dev/tty <(col -b)"'

### "nvim" as manpager
# export MANPAGER="nvim -c 'set ft=man' -"

### "less" as manpager
export MANPAGER="less"


# Load aliases and shortcuts if existent
### CHECK LUKE SMITH
#[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
#[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"


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



