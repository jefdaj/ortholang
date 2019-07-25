{ pkgs, stdenv, fetchurl, makeWrapper, mcl, fastme, psiblast-exb, diamond }:

# This is based on my psiblast-exb package, which in turn is based on the ncbi-blast one
# I'm not sure if this is the proper way to set libPath but it seems to work

# TODO include the newer diamond in the repo until nixpkgs updates it

let
  myPython = pkgs.python27Packages.python.withPackages (ps: with ps; [
    numpy
    scipy
  ]);

in stdenv.mkDerivation rec {
  name = "orthofinder-${version}";
  version = "2.3.3";
  src = fetchurl {
    url = "https://github.com/davidemms/OrthoFinder/releases/download/${version}/OrthoFinder-${version}_source.tar.gz";
    sha256 = "12llzc22k52h9f48irb5axhkx9mm6844cj1qbig9wvqi9da541ra";
  };
  buildInputs = [ makeWrapper ] ++ runDepends;
  runDepends = [
    #psiblast-exb # TODO remove in favor of diamond/mmseqs2?
    diamond
    fastme
    mcl
    myPython
  ];
  phases = "unpackPhase patchPhase installPhase";
  patches = [
    ./remove-results-dates.patch
  ];
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
    cp OrthoFinder-${version}_source/orthofinder/orthofinder.py "$exe"
    cp -r OrthoFinder-${version}_source/orthofinder/scripts $out/bin/
    cp -r OrthoFinder-${version}_source/orthofinder/tools $out/bin/
    cp OrthoFinder-${version}_source/orthofinder/config.json $out/bin/
    linker="$(cat $NIX_CC/nix-support/dynamic-linker)"
    echo "patching $exe"
    # patchelf --interpreter "$linker"  "$exe"
    # patchelf --set-rpath   "${psiblast-exb.libPath}" "$exe"
    # patchelf --shrink-rpath "$exe"
    wrapProgram "$exe" --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
}
