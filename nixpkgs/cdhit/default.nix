with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "cdhit-${version}";
  version = "4.6.8";
  src = fetchurl {
    url = "https://github.com/weizhongli/cdhit/archive/V${version}.tar.gz";
    sha256 = "0jarns7wpwyn2gipb11nf1hd1q87nx6lvzh5310194w4mbj8bmip";
  };
  installPhase = ''
    mkdir -p $out/bin
    make PREFIX=$out/bin install
  '';
}
