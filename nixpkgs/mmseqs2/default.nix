with import <nixpkgs> {};

# TODO can I enable AVX2 on the demo server, or is the CPU just too old??
# TODO replace perl with xxd?

let p = { stdenv, fetchurl, cmake, perl, zlib }:

stdenv.mkDerivation rec {
  name = "mmseqs2-${version}";
  version = "6-f5a1c";
  src = fetchurl {
    url = "https://github.com/soedinglab/MMseqs2/archive/${version}.tar.gz";
    sha256 = "11mm1m9dm7jwdsap4p7p6hl3ghqvxr2k0hjf1mp7jm1g988v79s1";
  };
  buildInputs = [ cmake perl zlib ];
  preConfigure = ''
    cmakeFlags="$cmakeFlags -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=$out"
  '';
};

in callPackage p {}
