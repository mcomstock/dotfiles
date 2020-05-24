{ stdenv
, lib
, fetchFromGitHub
, makeWrapper
, python38
, python38Packages
, soqt
, qt48Full
, xterm
, motif
, libxmi
, coin3d
, soxt
, transfig
, gfortran
}:

stdenv.mkDerivation {
  name = "auto-07p";
  version = "20200115";

  buildInputs = [
    qt48Full
    soqt
    xterm
    motif
    libxmi
    coin3d
    soxt
    transfig
    gfortran
  ];

  nativeBuildInputs = [
    makeWrapper
  ];

  pythonPath = [
    python38Packages.tkinter
  ];

  propagatedBuildInputs = [
    python38
    python38Packages.matplotlib
    python38Packages.ipython
  ];

  src = fetchFromGitHub {
    owner = "auto-07p";
    repo = "auto-07p";
    rev = "f170ec0028b73ab2ddd019438a476e317d4c2f90";
    sha256 = "1l0bwrqwfkp84lj4zw3185wv65zqvcbkiiysx7ch1qb0ij25gxj5";
  };

  postBuild = let
    binPath = lib.makeBinPath [
      python38
    ];
  in ''
    mkdir "$out"
    mkdir "$out/bin"

    cp -r ../source "$out/auto-dir"
    install bin/auto "$out/bin/auto-07p"

    wrapProgram "$out/bin/auto-07p" \
      --set AUTO_DIR "$out/auto-dir" \
      --set PYTHONPATH $PYTHONPATH \
      --prefix PATH : ${binPath}
  '';

  dontInstall = true;
}
