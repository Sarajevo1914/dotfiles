#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# disable ctrl-s and ctrl-q.
stty -ixon 

# allow you to cd into directory merely by tiping the directory name
shopt -s autocd 

HISTTIMEFORMAT="%F %T "
# Alias

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '


alias ..='cd ..'
alias ls='lsd -A --group-dirs first'
alias ll='lsd -lA --group-dirs first --total-size'
alias llt='lsd -lAt --group-dirs first --total-size'
alias lls='lsd -lAS --group-dirs first --total-size'
alias ns='nsxiv-rifle'
