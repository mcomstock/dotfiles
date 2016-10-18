#!/usr/bin/zsh

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

local username='%{%f$fg[red]%}%n%{%f%}'
local machine='%{%f$fg[green]%}%m%{%f%}'
local dir='%{%f$fg[cyan]%}%~%{%f%}'
local input='%{%f$fg[magenta]%}>%{%f%}'
local newline=$'\n'

autoload -Uz colors && colors
PROMPT="${username}@${machine} [${dir}]${newline}${input} "

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

# ls uses colors correctly with urxvt
alias ls='ls --color=auto'

# emacs in terminal
alias emacs='emacs -nw'

# tmux uses more colors
alias tmux='tmux -2'

# start firefox in private browsing mode
alias firefoxp='firefox --private-window'

# fish-style syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
