# with import <nixpkgs> {};
# let p =

# TODO can I enable AVX2 on the demo server, or is the CPU just too old??
# TODO replace perl with xxd?
# TODO why isn't the new version being used in sonicparanoid?

{ stdenv, fetchurl, cmake, perl, zlib }:

stdenv.mkDerivation rec {
  name = "mmseqs2-${version}";

  version = "1-c7a89"; # version for sonicparanoid
  #version = "6-f5a1c"; # official version

  src = fetchurl {
    # version required by sonicparanoid
    url = "https://bitbucket.org/salvocos/sonicparanoid/raw/55537a6e37a7767c125db18e2bb22059095c47cb/sonicparanoid/mmseqs2_src/mmseqs.tar.gz";
    sha256 = "0hrcky0jc2pn9gh8xsxknkr08fkm1xbmqwhhxq8rdvaygdjw4spw";

    # official version
    # url = "https://github.com/soedinglab/MMseqs2/archive/${version}.tar.gz";
    # sha256 = "11mm1m9dm7jwdsap4p7p6hl3ghqvxr2k0hjf1mp7jm1g988v79s1";
  };
  buildInputs = [ cmake perl zlib ];
  unpackPhase = ''
    mkdir -p $TMPDIR
    cd $TMPDIR
    mkdir ${name}
    cd ${name}
    tar xvzf ${src}
  '';
  preConfigure = ''
    cmakeFlags="$cmakeFlags -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=$out"
  '';
}

# ; in callPackage p {}
