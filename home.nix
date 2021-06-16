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
      direnv
      feh
      ffmpeg
      firefox-wayland
      gdb
      gnuplot
      grim
      htop
      imagemagick
      ispell
      krita
      lhasa
      mpv
      neofetch
      notify-desktop
      poppler_utils
      # Notes in case processing breaks: Processing requires the Oracle JDK rather than OpenJDK, but
      # now Oracle requires a login to download the JDK. However, I was able to get the right
      # version here:
      # https://download.oracle.com/otn-pub/java/jdk/8u271-b09/61ae65e088624f5aaa0b1d2d801acb16/jdk-8u271-linux-x64.tar.gz
      # by following this advice:
      # https://gist.github.com/wavezhang/ba8425f24a968ec9b2a8619d7c2d86a6#gistcomment-3112817 (copy
      # the download link that redirects to sign-on and replace "otn" with "otn-pub".
      # processing
      spotifyd
      ripgrep
      slurp
      texlive.combined.scheme-full
      tmux
      ttf-envy-code-r
      xfce.thunar
      zathura
      zip
    ] ++ (with unstable; [
      julia_16-bin
      python3
      spotify-tui
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

  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        size = 10.0;
        normal = {
          family = "monospace";
          style = "Regular";
        };
        bold = {
          family = "monospace";
          style = "Bold";
        };
        italic = {
          family = "monospace";
          style = "Italic";
        };
        bold_italic = {
          family = "monospace";
          style = "Bold Italic";
        };
      };
      # Colors (Solarized Dark)
      colors = {
        # Default colors
        primary = {
          background = "#002b36"; # base03
          foreground = "#839496"; # base0
        };
        # Cursor colors
        cursor = {
          text = "#002b36"; # base03
          cursor = "#839496"; # base0
        };
        # Normal colors
        normal = {
          black = "#073642"; # base02
          red = "#dc322f"; # red
          green = "#859900"; # green
          yellow = "#b58900"; # yellow
          blue = "#268bd2"; # blue
          magenta = "#d33682"; # magenta
          cyan = "#2aa198"; # cyan
          white = "#eee8d5"; # base2
        };
        # Bright colors
        bright = {
          black = "#586e75"; # base01
          red = "#cb4b16"; # orange
          green = "#586e75"; # base01
          yellow = "#657b83"; # base00
          blue = "#839496"; # base0
          magenta = "#6c71c4"; # violet
          cyan = "#93a1a1"; # base1
          white = "#fdf6e3"; # base3
        };
      };
    };
  };

  services.udiskie = {
    enable = true;
  };
}
