#!/usr/bin/zsh
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source .bash_profile

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

autoload -Uz colors && colors
local newline=$'\n'
local username='%{%f%F{33}%}%n%{%f%}'
local machine='%{%f%F{130}%}%m%{%f%}'
local dir='%{%f%F{144}%}%~%{%f%}'
# the input prompt is green if the last exit code was 0, red otherwise
local input='%{%f%}%(?.%{$fg[green]%}.%{$fg[red]%})>%{%f%}'
local time='%{%f%F{105}%}%D{%T}%{%f%}'
local date='%{%f%F{125}%}%D{%F}%{%f%}'
local exit_code='%(?.%{$fg[green]%}.%{$fg[red]%})%?%{%f%}'
local gray_at='%{%F{246}%}@%{%f%}'
local gray_lb='%{%F{246}%}[%{%f%}'
local gray_rb='%{%F{246}%}]%{%f%}'
local rprompt_string="${time} ${date} ${gray_lb}${exit_code}${gray_rb}"
local prompt_string="${username}${gray_at}${machine} ${dir}${newline}${input} "
if [[ -n $BUILDNAME || -n $MASTER_ROOT_INSTANCE ]]; then
    local bname='%{$fg[cyan]%}$BUILDNAME%{%f%}'
    local master_root='%{$fg[red]%}$MASTER_ROOT_INSTANCE%{%f%}'
    local prompt_string="${gray_lb}${bname} ${master_root}${gray_rb} ${prompt_string}"
fi
PROMPT="${prompt_string}"

RPROMPT="${rprompt_string}"

# up/down arrow only shows commands matching the current line
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search

# command completion
autoload -Uz compinit
compinit
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
