#!/usr/bin/zsh

# Fix "compinit: insecure directories" by running
# compaudit | xargs chmod g-w

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# use vi-style commands
bindkey -v

alias emacs="emacs -nw"
export GIT_EDITOR="emacs -nw"

# very short delay on escape key
KEYTIMEOUT=1

bindkey '^?' backward-delete-char
bindkey '^W' backward-kill-word
bindkey '^F' forward-char

# select and traverse words like bash
autoload -U select-word-style
select-word-style bash

# command completion
autoload -Uz compinit && compinit

# enable bash completions
# must be after compinit and before running the bash script
autoload -U +X bashcompinit && bashcompinit

[[ -e ~/.bash_profile ]] && source ~/.bash_profile

if command -v direnv &> /dev/null; then
    eval "$(direnv hook zsh)"
fi

# save history
HISTSIZE=1000000
if (( ! EUID )); then
    HISTFILE=~/.history_root
else
    HISTFILE=~/.history
fi
SAVEHIST=1000000
setopt inc_append_history
setopt share_history
setopt hist_reduce_blanks

# prompt stuff
setopt promptsubst

# version control information
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' check-for-staged-changes true
zstyle ':vcs_info:*' stagedstr '%{%f%F{green}%}▲%{%f%}'
zstyle ':vcs_info:*' unstagedstr '%{%f%F{red}%}▼%{%f%}'
zstyle ':vcs_info:git*' formats '(%b) %c%u'
zstyle ':vcs_info:git*' actionformats '(%b - %a) %c%u'

# use preexec and precmd to record the time it takes to run each command
preexec () {
    (( $#_elapsed > 1000 )) && set -A _elapsed $_elapsed[-1000,-1]
    (( $#_elapsed_formatted > 1000 )) && set -A _elapsed[-1000,-1]
    typeset -ig _start=SECONDS
}

precmd () {
    vcs_info

    if (( _start >= 0 )); then
        set -A _elapsed $_elapsed $(( SECONDS-_start ))
        _prev_seconds=$_elapsed[-1]
        ((_sec=_prev_seconds%60, _prev_seconds/=60, _min=_prev_seconds%60, _hrs=_prev_seconds/60))
        _timestamp=$(printf "%d:%02d:%02d" $_hrs $_min $_sec)
        set -A _elapsed_formatted $_elapsed_formatted $_timestamp
    fi
    _start=-1
}

vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

autoload -Uz colors && colors

local newline=$'\n'
local dir='%{%f$fg[cyan]%}[%~]%{%f%}'
local vc_info='%{%f$fg[magenta]%}${vcs_info_msg_0_}%{%f%}'
# the input prompt is blue if the last exit code was 0, red otherwise
local input='%{%f%}%(?.%{$fg[green]%}.%{$fg[red]%})>>%{%f%}'
local exit_code='%(?.%{$fg[green]%}.%{$fg[red]%})%?%{%f%}'
local elapsed='$(echo $_elapsed_formatted[-1])'
local time_info='%{%f$fg[blue]%}[$(echo $_elapsed_formatted[-1]) %D{%T %F}]%{%f%}'

PROMPT="${time_info}${dir}${vc_info}${newline}${exit_code} ${input} "
RPROMPT=""

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

typeset -A ZSH_HIGHLIGHT_STYLES

# Enable highlighters
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

# Override highlighter colors
ZSH_HIGHLIGHT_STYLES[default]='none'
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red'
ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=red,standout'
ZSH_HIGHLIGHT_STYLES[alias]='fg=green'
ZSH_HIGHLIGHT_STYLES[builtin]='fg=green'
ZSH_HIGHLIGHT_STYLES[function]='fg=green'
ZSH_HIGHLIGHT_STYLES[command]='fg=green'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=white,underline'
ZSH_HIGHLIGHT_STYLES[commandseparator]='none'
ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=red,bold'
ZSH_HIGHLIGHT_STYLES[path]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=white,underline'
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=green'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=green'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=green'
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[assign]='none'

ZSH_AUTOSUGGEST_USE_ASYNC=1

# change cursor shape based on vi mode
function zle-keymap-select zle-line-init {
    case $KEYMAP in
        vicmd)
            echo -ne "\e[2 q"
            ;;
        viins|main)
            echo -ne "\e[6 q"
            ;;
    esac

    zle reset-prompt
    zle -R
}

# reset cursor shape to block when leaving command mode
function zle-line-finish {
    echo -ne "\e[2 q"
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

# autosuggestions
# [[ -e ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# fish-style syntax highlighting
# [[ -e ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
