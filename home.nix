{ pkgs, ... }:

let
  # Note: to get this to work, you must run
  # nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
  # nix-channel --update
  unstable = import <unstable> {};
in {
  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    feh
    fontconfig
    htop
    imagemagick
    mpv
    neofetch
    tmux
    ttf-envy-code-r
    xfce.thunar
    zathura
  ] ++ (with unstable; [
    spotify-tui
    spotifyd
    udiskie
  ]);

  xdg.configFile."fontconfig/fonts.conf".source = ~/dotfiles/fonts.conf;
  xdg.configFile."sway/config".source = ~/dotfiles/sway/config;
  xdg.configFile."waybar/config".source = ~/dotfiles/waybar/config;
  xdg.configFile."waybar/style.css".source = ~/dotfiles/waybar/style.css;
  xdg.configFile."spotifyd/spotifyd.conf".source = ~/dotfiles/spotifyd/spotifyd.conf;

  programs.emacs = {
    enable = true;
  };

  programs.firefox = {
    enable = true;
  };

  programs.termite = {
    enable = true;
    allowBold = true;
    audibleBell = false;
    backgroundColor = "rgba(0, 0, 0, 1)";
    foregroundColor = "#bf699d";
    clickableUrl = true;
    font = "Envy Code R 10";
    cursorBlink = "off";

    colorsExtra = ''
      color0 = #000000
      color1 = #bf176d
      color2 = #73c07a
      color3 = #c07c63
      color4 = #068bbf
      color5 = #908ec0
      color6 = #02abc0
      color7 = #bf699d
      color8 = #585858
      color9 = #bf176d
      color10 = #73c07a
      color11 = #c07c63
      color12 = #068bbf
      color13 = #908ec0
      color14 = #02abc0
      color15 = #bf699d
    '';
  };

  services.udiskie = {
    enable = true;
  };
}
