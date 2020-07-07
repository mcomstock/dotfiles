{ mkDerivation
, makeWrapper
, autoPatchelfHook
, cmake
, fetchFromGitHub
, git
, qtbase
, qtsvg
, freeimage
, freeglut
, mesa
, xorg
, libGL
, libGLU
, freetype
, glew
}:

mkDerivation rec {
  name = "trenchbroom";
  version = "60ba694ccad407d83a6780a6a2a7af612a7878a3";

  src = fetchFromGitHub {
    owner = "kduske";
    repo = "TrenchBroom";
    rev = version;
    sha256 = "0yxrpd37jqhn3bbydr9sab9n87gqf34wjhrbps6cj63742nc735l";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [
    autoPatchelfHook
    cmake
    git
    makeWrapper
  ];

  buildInputs = [
    qtbase
    qtsvg
    freeimage
    freeglut
    mesa
    xorg.libXxf86vm
    xorg.libX11
    libGL
    libGLU
    freetype
    # This should theoretically create a Wayland-compatible version of GLEW:
    # glew-wayland = pkgs.glew.overrideAttrs ( oldAttrs: rec { makeFlags = ["SYSTEM=linux-egl"]; } );
    # It still crashed, but maybe will work someday so XWayland is no longer necessary here.
    glew
  ];

  dontConfigure = true;
  dontBuild = true;
  dontInstall = true;

  # GLEW breaks when using Wayland for now, so force X.
  qtWrapperArgs = [ "--set QT_QPA_PLATFORM xcb" ];

  preFixup = ''
    mkdir build
    cd build

    cmake .. -DCMAKE_BUILD_TYPE=Release
    cmake --build . --target TrenchBroom-nomanual

    cd ..
    mkdir "$out"
    mkdir "$out/bin"
    cp -r build/app/defaults "$out/bin/defaults"
    cp -r build/app/fonts "$out/bin/fonts"
    cp -r build/app/images "$out/bin/images"
    cp -r build/app/games "$out/bin/games"
    cp -r build/app/stylesheets "$out/bin/stylesheets"
    cp -r build/app/shader "$out/bin/shader"
    install build/app/trenchbroom-nomanual "$out/bin/trenchbroom"
  '';
}
