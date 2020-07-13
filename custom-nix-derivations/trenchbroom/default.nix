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
, ninja
, xorg
, libGL
, libGLU
, freetype
, glew
}:

let
  # glew-egl = glew.overrideAttrs (oldAttrs: {
  #   pname = "glew-egl";
  #   makeFlags = ["SYSTEM=linux-egl"];
  # });
in mkDerivation rec {
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
    ninja
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
    # The glew-egl versions should supposedly work with Wayland, but it still crashes.
    # glew-egl
    glew
  ];

  dontConfigure = true;
  dontBuild = true;
  dontInstall = true;

  # GLEW breaks when using Wayland for now, so force X.
  qtWrapperArgs = [
    # "--prefix LD_LIBRARY_PATH : ${glew-egl.out}/lib"
    "--prefix LD_LIBRARY_PATH : ${glew.out}/lib"
    # Options: wayland-egl, wayland, wayland-xcomposite-egl, wayland-xcomposite-glx, eglfs, linuxfb, minimal, minimalegl, offscreen, vnc, xcb
    "--set QT_QPA_PLATFORM xcb"
  ];

  preFixup = ''
    mkdir build
    cd build

    cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release
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
