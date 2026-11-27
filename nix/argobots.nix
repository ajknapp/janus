{ lib
, stdenv
, autoconf
, automake
, libtool
, argobots-src
}:

stdenv.mkDerivation {
  pname = "argobots";
  version = "1.2";
  src = argobots-src;

  nativeBuildInputs = [
    autoconf
    automake
    libtool
  ];

  preConfigure = ''
    ./autogen.sh
  '';

  configureFlags = [
    "--enable-fast=O3"
    "--disable-static"
    "--enable-shared"
  ];

  enableParallelBuilding = true;

  meta = with lib; {
    description = "Argobots: lightweight, low-level threading and tasking framework";
    homepage = "https://www.argobots.org/";
    license = licenses.bsd3;
    platforms = platforms.unix;
  };
}
