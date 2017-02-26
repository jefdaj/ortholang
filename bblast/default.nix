with import <nixpkgs> {};
with pythonPackages;
let bblast =

{ pythonPackages }:

buildPythonPackage {
  name = "bblast-0.1";
  namePrefix = "";
  src = ./.;
  propagatedBuildInputs = [
    ncbi-blast
    docopt
    parallel
    progressbar
  ];
  doCheck = false;
  dontStrip = true;
}

; in callPackage bblast {}
