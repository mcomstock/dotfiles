#!/usr/bin/zsh
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# command completion
autoload -Uz compinit && compinit

# enable bash completions
# must be after compinit and before running the bash script
autoload -U +X bashcompinit && bashcompinit

source ~/.bash_profile

# save history
HISTSIZE=1000000
if (( ! EUID )); then
    HISTFILE=~/.history_root
else
    HISTFILE=~/.history
fi
SAVEHIST=1000000
setopt SHARE_HISTORY

# prompt stuff
setopt promptsubst

# use preexec and precmd to record the time it takes to run each command
preexec () {
    (( $#_elapsed > 1000 )) && set -A _elapsed $_elapsed[-1000,-1]
    typeset -ig _start=SECONDS
}

precmd () {
    (( _start >= 0 )) && set -A _elapsed $_elapsed $(( SECONDS-_start ))
    _start=-1
}

autoload -Uz colors && colors
local newline=$'\n'
local _lineup=$'\e[1A'
local _linedown=$'\e[1B'
local lineup='%{${_lineup}%}'
local linedown='%{${_linedown}%}'
local username='%{%f%F{35}%}%n%{%f%}'
local machine='%{%f%F{75}%}%m%{%f%}'
local dir='%{%f%F{222}%}%~%{%f%}'
# the input prompt is blue if the last exit code was 0, red otherwise
local input='%{%f%}%(?.%{$fg[cyan]%}.%{%F{197}%})❯❯%{%f%}'
local time='%{%f%F{75}%}%D{%T}%{%f%}'
local date='%{%f%F{35}%}%D{%F}%{%f%}'
local exit_code='%(?.%{$fg[cyan]%}.%{%F{197}%})%?%{%f%}'
local gray_at='%{%F{246}%}@%{%f%}'
local gray_lb='%{%F{246}%}[%{%f%}'
local gray_rb='%{%F{246}%}]%{%f%}'
local elapsed='%{%F{135}%}$(echo $_elapsed[-1])%{%f%}'

local rprompt_string="${lineup}${elapsed} ${time} ${date}${linedown}"
local prompt_string="${username}${gray_at}${machine} ${dir}${newline}${exit_code} ${input} "

if [[ -n $BUILDNAME || -n $MASTER_ROOT_INSTANCE ]]; then
    local color_buildname='%{$fg[cyan]%}$(echo $BUILDNAME | sed -e "s/\(.*\)\/\(.*\)/\x1b[38;5;6m\1\x1b[38;5;246m\/\x1b[38;5;135m\2/g")%{%f%}'
    local master_root='%{$fg[red]%}$MASTER_ROOT_INSTANCE%{%f%}'
    local prompt_string="${gray_lb}${color_buildname} ${master_root}${gray_rb} ${prompt_string}"
fi

PROMPT="${prompt_string}"

RPROMPT="${rprompt_string}"

# up/down arrow only shows commands matching the current line
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search
bindkey "^P" up-line-or-beginning-search
bindkey "^N" down-line-or-beginning-search

# tab twice to use arrow keys
zstyle ':completion:*' menu select
# autocomplete aliases
setopt COMPLETE_ALIASES

# select and traverse words like bash
autoload -U select-word-style
select-word-style bash

# These are set in bashrc (or not at all at work)
if [[ $AX_HOME != *athenax* ]]; then
    # ls uses colors correctly with urxvt
    alias ls='ls --color=auto'

    # emacs in terminal
    alias emacs='emacs -nw'

    # tmux uses more colors
    alias tmux='tmux -2'

    # start firefox in private browsing mode
    alias firefoxp='firefox --private-window'
fi

# fish-style syntax highlighting
if [[ -e /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
elif [[ -e /home/mcomstock/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /home/mcomstock/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# Enable highlighters
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

# Override highlighter colors
ZSH_HIGHLIGHT_STYLES[default]=none
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=197
ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=197,standout
ZSH_HIGHLIGHT_STYLES[alias]=fg=cyan
ZSH_HIGHLIGHT_STYLES[builtin]=fg=cyan
ZSH_HIGHLIGHT_STYLES[function]=fg=cyan
ZSH_HIGHLIGHT_STYLES[command]=fg=cyan
ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
ZSH_HIGHLIGHT_STYLES[commandseparator]=none
ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009
ZSH_HIGHLIGHT_STYLES[path]=fg=222
ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=fg=35
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=fg=35
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=fg=135
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=magenta
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=magenta
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=161
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=161
ZSH_HIGHLIGHT_STYLES[assign]=none
