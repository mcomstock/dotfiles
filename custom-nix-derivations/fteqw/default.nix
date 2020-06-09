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
  version = "r5702";

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
    rev = "5702";
    sha256 = "1g31b27g62hwi19xnmbcrydawh6cdqf7dl6bidklmfr5ipm4sx3a";
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
