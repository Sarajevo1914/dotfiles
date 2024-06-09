# Setings

PS1='[\u@\h \W]\$ '


# Load aliases and shortcuts if existent
### CHECK LUKE SMITH
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# disable ctrl-s and ctrl-q.
stty -ixon
shopt -s autocd
shopt -s cdspell
shopt -s expand_aliases

# ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

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

eval "$(zoxide init bash)"
