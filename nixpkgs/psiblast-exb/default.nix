{stdenv, fetchgit, patchelf, zlib, bzip2}:

# This is based on the regular ncbi-blast-2.5.0 nix package.
# I replaced src and edited unpackPhase + installPhase.

stdenv.mkDerivation rec {
  name = "psiblast-exb-${version}";
  version = "2.5.0";
  src = fetchgit {
    url = "git://github.com/kyungtaekLIM/PSI-BLASTexB";
    rev = "e16ded9280bbe8dfc97f29af697b3dec27616c70";
    sha256 = "0p6s1yp0z001h6fz246h0msjihkxiaqrm39n41hiz3nq41gmwk5k";
  };
  buildInputs = [ patchelf ];
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
    cp -r $src/binaries/ncbi-blast-* $TMP
    chmod +w $TMP -R
  '';
  installPhase = ''
    linker="$(cat $NIX_CC/nix-support/dynamic-linker)"
    cd $TMP/ncbi-blast-*
    for exe in bin/*; do
      [[ "$exe" =~ .*\.pl$ ]] && continue
      [[ "$exe" =~ .*\.py$ ]] && continue
      echo "patching $exe"
      patchelf --interpreter "$linker"  "$exe"
      patchelf --set-rpath   "$libPath" "$exe"
      # patchelf --shrink-rpath "$exe"
    done
    mkdir $out; cp -R * $out; rm $out/README; rm $out/LICENSE
  '';
}
