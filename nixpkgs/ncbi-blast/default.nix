# see http://nixos.org/patchelf.html
# and firefox-bin (suggested on mailing list)

# TODO update to a much newer version?

{stdenv, fetchurl, patchelf, zlib, bzip2}:

let
  ftpSite = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+";
  arch = if stdenv.system == "x86_64-darwin"
    then "x64-macosx"
    else "x64-linux";

  # TODO fill in more completely:
  # arch = if stdenv.system == "i686-linux"
  #  then "linux-i686"
  #  else "linux-x86_64";
  # src = if stdenv.is64bit
  #  then ...
  #  else ...

in stdenv.mkDerivation rec {
  version = "2.9.0";
  name = "ncbi-blast-${version}";
  src = fetchurl {
    url = "${ftpSite}/${version}/${name}+-${arch}.tar.gz";
    sha256 = "1jc61q9gnpnh1wqz1zqmvlgknf6a4ma26jnkpd13liq2wj5xzdyc";
  };
  buildInputs = if stdenv.hostPlatform.system == "x86_64-darwin" then [] else [ patchelf ];
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
    cd $TMP/ncbi-blast-*
  '' + (if stdenv.hostPlatform.system == "x86_64-darwin" then "" else ''
    linker="$(cat $NIX_CC/nix-support/dynamic-linker)"
    for exe in bin/*; do
      [[ "$exe" =~ .*\.pl$ ]] && continue
      patchelf --interpreter "$linker"  "$exe"
      patchelf --set-rpath   "$libPath" "$exe"
      # patchelf --shrink-rpath "$exe"
    done
  '') + ''
    mkdir $out; cp -R * $out
  '';
}
