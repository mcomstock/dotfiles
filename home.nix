{ pkgs, ... }:

let
  # Note: to get this to work, you must run
  # nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
  # nix-channel --update
  unstable = import <unstable> {};

  # auto-07p = pkgs.callPackage custom-nix-derivations/auto-07p/default.nix {};
  # fteqw = pkgs.callPackage custom-nix-derivations/fteqw/default.nix {};
  # qmk-cli = pkgs.callPackage custom-nix-derivations/qmk-cli/default.nix {};
  # trenchbroom = pkgs.libsForQt5.callPackage custom-nix-derivations/trenchbroom/default.nix {};
in {
  nixpkgs.config.allowUnfree = true;

  home = {
    username = "max";
    stateVersion = "20.09";
    homeDirectory = "/home/max";

    packages = with pkgs; [
      clang
      clang-tools
      corefonts
      direnv
      feh
      ffmpeg
      firefox-wayland
      fontconfig
      gdb
      gnuplot
      htop
      imagemagick
      krita
      lhasa
      mpv
      neofetch
      # Notes in case processing breaks: Processing requires the Oracle JDK rather than OpenJDK, but
      # now Oracle requires a login to download the JDK. However, I was able to get the right
      # version here:
      # https://download.oracle.com/otn-pub/java/jdk/8u271-b09/61ae65e088624f5aaa0b1d2d801acb16/jdk-8u271-linux-x64.tar.gz
      # by following this advice:
      # https://gist.github.com/wavezhang/ba8425f24a968ec9b2a8619d7c2d86a6#gistcomment-3112817 (copy
      # the download link that redirects to sign-on and replace "otn" with "otn-pub".
      processing
      ripgrep
      texlive.combined.scheme-full
      tmux
      ttf-envy-code-r
      xfce.thunar
      zathura
      zip
    ] ++ (with unstable; [
      python3
      spotify-tui
      spotifyd
      texlab
      udiskie
      ums
    ]) ++ [
      # auto-07p
      # fteqw
      # qmk-cli
      # trenchbroom
    ];
  };

  xdg.configFile."fontconfig/fonts.conf".source = ~/dotfiles/fonts.conf;
  xdg.configFile."sway/config".source = ~/dotfiles/sway/config;
  xdg.configFile."waybar/config".source = ~/dotfiles/waybar/config;
  xdg.configFile."waybar/style.css".source = ~/dotfiles/waybar/style.css;
  xdg.configFile."spotifyd/spotifyd.conf".source = ~/dotfiles/spotifyd/spotifyd.conf;
  xdg.configFile."zathura/zathurarc".source = ~/dotfiles/zathura/zathurarc;

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };

  programs.termite = {
    enable = true;
    allowBold = true;
    audibleBell = false;
    backgroundColor = "rgba(0, 0, 0, 1)";
    foregroundColor = "#bf699d";
    clickableUrl = true;
    font = "Monospace 10";
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
