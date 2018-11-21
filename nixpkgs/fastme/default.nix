{ stdenv, writeScript }:

stdenv.mkDerivation {
  name = "fastme";
  # TODO can it be downloaded without the email thing?
  src = ./fastme-2.1.5.tar.gz;
  builder = writeScript "builder.sh" ''
    source $stdenv/setup
    cd $TMPDIR
    tar xvzf $src
    mkdir -p $out/bin
    exe="$out/bin/fastme"
    cp fastme-*/binaries/fastme-2.1.5-linux64 "$exe"
    linker="$(cat $NIX_CC/nix-support/dynamic-linker)"
    patchelf --interpreter "$linker"  "$exe"
  '';
}
