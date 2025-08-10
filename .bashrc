# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

stty -ixon
shopt -s autocd
shopt -s cdspell
shopt -s expand_aliases
bind "set completion-ignore-case on"

export HISTFILE="${XDG_CONFIG_HOME:-$HOME/.config}/shell/bash_history"
HISTSIZE=10000000
HISTFILESIZE=10000000
shopt -s histappend
export HISTCONTROL=ignoreboth:erasedups

# Load files if exist
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Init zoxide if exist
command -v zoxide >/dev/null && eval "$(zoxide init bash)"
