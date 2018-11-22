{ pkgs, stdenv, fetchurl, makeWrapper, mcl, fastme, psiblast-exb, diamond }:

# This is based on my psiblast-exb package, which in turn is based on the ncbi-blast one
# I'm not sure if this is the proper way to set libPath but it seems to work

stdenv.mkDerivation rec {
  name = "orthofinder-${version}";
  version = "2.2.6";
  src = fetchurl {
    url = "https://github.com/davidemms/OrthoFinder/releases/download/v${version}/OrthoFinder-${version}.tar.gz";
    sha256 = "1avbpswkv8ggmvqgbp08z1dl5n3h21wmqm87pxgk2m28nw013ikv";
  };
  buildInputs = [ makeWrapper ] ++ runDepends;
  runDepends = [
    psiblast-exb # TODO remove in favor of diamond/mmseqs2?
    diamond
    fastme
    mcl
  ];
  phases = "unpackPhase installPhase";
  unpackPhase = ''
    source $stdenv/setup
    cd $TMPDIR
    tar xvzf $src
  '';
  installPhase = ''
    mkdir -p $out/bin
    exe="$out/bin/orthofinder"
    cp OrthoFinder-${version}/orthofinder "$exe"
    linker="$(cat $NIX_CC/nix-support/dynamic-linker)"
    echo "patching $exe"
    patchelf --interpreter "$linker"  "$exe"
    patchelf --set-rpath   "${psiblast-exb.libPath}" "$exe"
    patchelf --shrink-rpath "$exe"
    wrapProgram "$exe" --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
}
