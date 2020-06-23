{ pkgs
, stdenv
, fetchFromGitHub
, python3Packages
}:

let
  hjson = pkgs.callPackage ../hjson/default.nix {};
in python3Packages.buildPythonApplication rec {
  pname = "qmk-cli";
  version = "0.0.35";

  src = fetchFromGitHub {
    owner = "qmk";
    repo = "qmk_cli";
    rev = version;
    sha256 = "16cl9b6cgcqdyhybqnkv34xnjwym7szlh2d60ncb8arlwqpdzb0m";
  };

  propagatedBuildInputs = [
    python3Packages.appdirs
    python3Packages.argcomplete
    python3Packages.colorama
    python3Packages.flake8
    python3Packages.nose2
    python3Packages.yapf
    hjson
  ];

  preConfigure = ''
    export QMK_HOME="/home/max/repos/qmk_firmware"
  '';

  makeWrapperArgs = ["--set QMK_HOME /home/max/repos/qmk_firmware"];
}
