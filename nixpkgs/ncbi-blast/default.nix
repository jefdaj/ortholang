# see http://nixos.org/patchelf.html
# and firefox-bin (suggested on mailing list)

# TODO update to a much newer version?

{stdenv, fetchurl, zlib, bzip2}:

let
  ftpSite = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+";
  arch = "x64-linux";
  # arch = if stdenv.system == "i686-linux"
  #  then "linux-i686"
  #  else "linux-x86_64";
  # src = if stdenv.is64bit
  #  then ...
  #  else ...

in stdenv.mkDerivation rec {
  version = "2.2.31";
  name = "ncbi-blast-${version}";
  src = fetchurl {
    url = "${ftpSite}/${version}/${name}+-${arch}.tar.gz";
    sha256 = "027lwjc7vac32q7s9dxvzc9xqhvlk1w4v9kndkqqwbna3cg9aarj";
  };
  dontStrip = 1;
  libPath = stdenv.lib.makeLibraryPath [
      stdenv.cc.cc
      zlib
      bzip2
    ] + ":" + stdenv.lib.makeSearchPath "lib64" [
      stdenv.cc.cc
      zlib
      bzip2
    ];
  phases = "unpackPhase installPhase";
  unpackPhase = ''
    source $stdenv/setup
    cd $TMP
    tar xzf $src
  '';
  installPhase = ''
    linker="$(cat $NIX_CC/nix-support/dynamic-linker)"
    cd $TMP/ncbi-blast-*
    for exe in bin/*; do
      [[ "$exe" =~ .*\.pl$ ]] && continue
      patchelf --interpreter "$linker"  "$exe"
      patchelf --set-rpath   "$libPath" "$exe"
      # patchelf --shrink-rpath "$exe"
    done
    mkdir $out; cp -R * $out
  '';
}
