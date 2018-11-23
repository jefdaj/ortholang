{ pkgs, stdenv, fetchurl, makeWrapper, mcl, fastme, psiblast-exb, diamond }:

# This is based on my psiblast-exb package, which in turn is based on the ncbi-blast one
# I'm not sure if this is the proper way to set libPath but it seems to work

# TODO include the newer diamond in the repo until nixpkgs updates it

stdenv.mkDerivation rec {
  name = "orthofinder-${version}";
  version = "2.3.1";
  src = fetchurl {
    url = "https://github.com/davidemms/OrthoFinder/releases/download/v${version}-beta/OrthoFinder-${version}.tar.gz";
    sha256 = "08mq8aa42c24w967zfb478fswq8h5fb4crvlsi1h8jls8kr0bqhz";
  };
  buildInputs = [ makeWrapper ] ++ runDepends;
  runDepends = [
    #psiblast-exb # TODO remove in favor of diamond/mmseqs2?
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
  # TODO why does the libPath come from PsiBlast?
  # TODO need to add config.json to the same dir... and maybe other stuff at the same time?
  installPhase = ''
    mkdir -p $out/bin
    exe="$out/bin/orthofinder"
    cp OrthoFinder-${version}/orthofinder "$exe"
    cp OrthoFinder-${version}/config.json $out/bin/
    linker="$(cat $NIX_CC/nix-support/dynamic-linker)"
    echo "patching $exe"
    patchelf --interpreter "$linker"  "$exe"
    patchelf --set-rpath   "${psiblast-exb.libPath}" "$exe"
    patchelf --shrink-rpath "$exe"
    wrapProgram "$exe" --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
}
