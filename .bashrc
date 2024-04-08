# # ~/.bashrc
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

export EDITOR="nvim"

# Load aliases and shortcuts if existent
### CHECK LUKE SMITH
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"

# Add all directories in `~/.local/bin` to $PATH
export PATH="$PATH:$(find ~/.local/bin -type d | paste -sd ':' -)"


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

# foot terminal shell integration
# usint ctrl shift n open a new terminal in the current working directory
osc7_cwd() {
    local strlen=${#PWD}
    local encoded=""
    local pos c o
    for (( pos=0; pos<strlen; pos++ )); do
        c=${PWD:$pos:1}
        case "$c" in
            [-/:_.!\'\(\)~[:alnum:]] ) o="${c}" ;;
            * ) printf -v o '%%%02X' "'${c}" ;;
        esac
        encoded+="${o}"
    done
    printf '\e]7;file://%s%s\e\\' "${HOSTNAME}" "${encoded}"
}
PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }osc7_cwd



if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    export MOZ_ENABLE_WAYLAND=1
fi

export PATH=/sbin/marksman:$PATH

eval "$(zoxide init bash)"
