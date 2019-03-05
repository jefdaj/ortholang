# TODO replace perl with xxd?
# TODO why isn't the new version being used in sonicparanoid?
# TODO include a custom check for AVX2 to re-enable it when a user's machine is capable
# TODO is an old version actually required by sonicparanoid?

{ stdenv, fetchurl, cmake, perl, zlib, unzip }:

stdenv.mkDerivation rec {
  name = "mmseqs2-${version}";

  # version = "1-c7a89"; # version for sonicparanoid
  version = "6-f5a1c"; # latest release version

  # src = ./mmseqs;
  src = fetchurl {
    # latest master branch as of march 5
    # (rev 67b4ca0708a5cd2a0f87220b97f11e562f6c7842)
    url = "https://github.com/soedinglab/MMseqs2/archive/master.zip";
    sha256 = "143i31i90gk1vry9cdqc68yskcr78rbmfk6wlhxlzlfgbr7mffcv";

    # latest official release
    # url = "https://github.com/soedinglab/MMseqs2/archive/${version}.tar.gz";
    # sha256 = "11mm1m9dm7jwdsap4p7p6hl3ghqvxr2k0hjf1mp7jm1g988v79s1";

    # version required by sonicparanoid
    # url = "https://bitbucket.org/salvocos/sonicparanoid/raw/55537a6e37a7767c125db18e2bb22059095c47cb/sonicparanoid/mmseqs2_src/mmseqs.tar.gz";
    # sha256 = "0hrcky0jc2pn9gh8xsxknkr08fkm1xbmqwhhxq8rdvaygdjw4spw";
  };
  buildInputs = [ cmake perl zlib unzip ];
  #for when the src is a tarball:
  #   unpackPhase = ''
  #     # bump
  #     mkdir -p $TMPDIR
  #     cd $TMPDIR
  #     mkdir ${name}
  #     cd ${name}
  #     tar xvzf ${src}
  #   '';
  postUnpack = ''
    patchShebangs MMseqs2-master/cmake MMseqs2-master/lib
  '';
  preConfigure = ''
    cmakeFlags="$cmakeFlags -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=$out -DHAVE_AVX_EXTENSIONS=0 -DHAVE_AVX2_EXTENSIONS=0"
  '';
}
