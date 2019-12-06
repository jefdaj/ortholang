{ stdenv, fetchgit, python27, writeScript, makeWrapper }:

let
  myPython = python27.withPackages (ps: with ps; [
    biopython
  ]);

in stdenv.mkDerivation {
  name = "justorthologs-022ebb4"; # March 2019
  src = fetchgit {
    url = "https://github.com/ridgelab/JustOrthologs.git";
    rev = "022ebb42aca1aff21a42e800d998440e0a49aaee";
    sha256 = "0n82j3bnd45hk9bcp13arwnp5dg8sx9qc16aalvnwvpwcyiharx8";
  };
  buildInputs = [
    makeWrapper
    myPython
  ];
  builder = writeScript "builder.sh" ''
    source ${stdenv}/setup
    mkdir -p $out/bin
    cp $src/*.py $out/bin
    for py in $out/bin/*; do
      wrapProgram "$py" --set PYTHONPATH "${myPython}/${myPython.sitePackages}"
    done
  '';
}
