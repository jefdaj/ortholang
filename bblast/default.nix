with import <mypkgs>;
let bblast =

{ pythonPackages }:

buildPythonPackage {
  name = "bblast-0.1";
  namePrefix = "";
  src = ./.;
  propagatedBuildInputs = with pythonPackages; [
    ncbi-blast
    docopt
    parallel
    progressbar
  ];
  doCheck = false;
  dontStrip = true;
}

; in myCall bblast
