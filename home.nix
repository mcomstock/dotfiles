{ pkgs, ... }:

{
  # nixpkgs.config.allowUnfree = true;

  home.packages = [
    pkgs.git
    pkgs.htop
    pkgs.termite
    pkgs.tmux
  ];

  programs.emacs = {
    enable = true;
  };

  programs.firefox = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    defaultKeymap = "viins";

    history = {
      share = true;
      size = 1000000;
    };

    initExtraBeforeCompInit = ''
      # very short delay on escape key
      KEYTIMEOUT=1

      bindkey '^?' backward-delete-char
      bindkey '^W' backward-kill-word
      bindkey '^F' forward-char

      # select and traverse words like bash
      autoload -U select-word-style
      select-word-style bash
    '';

    # Look into builtIns.readFile to maybe move this.
    initExtra = ''
      autoload -U +X bashcompinit && bashcompinit
      setopt inc_append_history
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

      autoload -Uz colors && colors

      local newline=$'\n'
      local dir='%{%f$fg[cyan]%}[%~]%{%f%}'
      local vc_info='%{%f$fg[magenta]%}''${vcs_info_msg_0_}%{%f%}'
      # the input prompt is blue if the last exit code was 0, red otherwise
      local input='%{%f%}%(?.%{$fg[green]%}.%{$fg[red]%})>>%{%f%}'
      local exit_code='%(?.%{$fg[green]%}.%{$fg[red]%})%?%{%f%}'
      local elapsed='$(echo $_elapsed_formatted[-1])'
      local time_info='%{%f$fg[blue]%}[$(echo $_elapsed_formatted[-1]) %D{%T %F}]%{%f%}'

      PROMPT="''${time_info}''${dir}''${vc_info}''${newline}''${exit_code} ''${input} "

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
    '';

    plugins = [
      {
        name = "zsh-syntax-highlighting";
        file = "zsh-syntax-highlighting.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "0.7.1";
          # nix-prefetch-url --unpack https://github.com/zsh-users/zsh-syntax-highlighting/archive/0.7.1.tar.gz
          sha256 = "03r6hpb5fy4yaakqm3lbf4xcvd408r44jgpv4lnzl9asp4sb9qc0";
        };
      }
    ];

    # autosuggestions = {
    #   enable = true;
    # };

    # syntaxHighlighting = {
    #   enable = true;
    # };

    shellAliases = {
      emacs = "emacs -nw";
    };
  };
}
