{ stdenv
, fetchsvn
, autoPatchelfHook
, pkgconfig
, alsaLib
, freetype
, gnutls
, libGL
, libGLU
, libjpeg_original
, libopus
, libpng
, libvorbis
, mesa
, SDL2
, speex
, xorg
, zlib
}:

stdenv.mkDerivation {
  name = "fteqw";
  version = "r5696";

  nativeBuildInputs = [
    autoPatchelfHook
    pkgconfig
  ];

  buildInputs = [
    alsaLib
    freetype
    gnutls
    libGL
    libGLU
    libjpeg_original
    libopus
    libpng
    libvorbis
    mesa
    SDL2
    speex
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    zlib
  ];

  src = fetchsvn {
    url = "https://svn.code.sf.net/p/fteqw/code/trunk";
    rev = "5696";
    sha256 = "05h9f359qyx4d3rjvlba84p4fir5jmpralrc2s9jy098d14r1j5i";
  };

  # The libopus derivation provides the opus.h file under the opus directory, rather than at the top
  # level as this file seems to expect.
  postPatch = ''
    substituteInPlace engine/client/snd_dma.c --replace '"opus.h"' '"opus/opus.h"'
  '';

  dontBuild = true;

  preInstall = ''
    cd engine
  '';

  installTargets = "sv-rel gl-rel qcc-rel";

  postInstall = ''
    cd ..
    mkdir "$out/bin"
    install engine/release/fteqw-gl "$out/bin/fteqw-gl"
    install engine/release/fteqw-sv "$out/bin/fteqw-sv"
    install engine/release/fteqcc "$out/bin/fteqcc"
  '';

  postFixup = ''
    patchelf --add-needed "libSDL2-2.0.so.0" "$out/bin/fteqw-gl"
    patchelf --add-needed "libGL.so.1" "$out/bin/fteqw-gl"
    patchelf --add-needed "libgnutls.so" "$out/bin/fteqw-gl"

    patchelf --add-needed "libgnutls.so" "$out/bin/fteqw-sv"
  '';

  # keepDebugInfo = true;
  # enableDebugging = true;
  # dontStrip = true;
}
