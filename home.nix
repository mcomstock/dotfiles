{ pkgs, ... }:

{
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

    autosuggestions = {
      enable = true;
    };

    syntaxHighlighting.enable = true;
  };
}
