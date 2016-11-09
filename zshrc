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
local username='%{%f$fg[red]%}%n%{%f%}'
local machine='%{%f$fg[green]%}%m%{%f%}'
local dir='%{%f$fg[cyan]%}%~%{%f%}'
local input='%{%f$fg[magenta]%}>%{%f%}'
local newline=$'\n'
if [[ -n $BUILDNAME || -n $MASTER_ROOT_INSTANCE ]]; then
    local bname='%{$fg[cyan]%}$BUILDNAME%{$reset_color%}'
    local master_root='%{$fg[red]%}$MASTER_ROOT_INSTANCE%{$reset_color%}'
    local prompt_string="%{%f$fg[green]%}[${bname} ${master_root}%{$fg[green]%}] %{$fg_bold[magenta]%}%n@%m %{$fg_bold[yellow]%}%~${newline}%{%b$fg[green]%}> %{%f%}"
else
    local prompt_string="${username}@${machine} [${dir}]${newline}${input} "
fi
PROMPT="${prompt_string}"

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
