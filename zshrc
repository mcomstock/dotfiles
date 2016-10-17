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

local bname='%{$fg[cyan]%}$BUILDNAME%{$reset_color%}'
local master_root='%{$fg[red]%}$MASTER_ROOT_INSTANCE%{$reset_color%}'

autoload -Uz colors && colors
PROMPT="%{%f$fg[green]%}[${bname} ${master_root}%{$fg[green]%}] %{$fg_bold[magenta]%}%m %{$fg_bold[yellow]%}%~
%{%b$fg[green]%}> %{%f%}"
