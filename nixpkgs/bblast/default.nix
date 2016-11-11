{ pythonPackages, ncbi-blast, parallel }:

pythonPackages.buildPythonPackage {
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
