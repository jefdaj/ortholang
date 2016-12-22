{ pkgs, pythonPackages }:

pythonPackages.buildPythonPackage rec {
  name = "biopython-${version}";
  version = "1.67";
  src = pkgs.fetchurl {
    url = "http://biopython.org/DIST/biopython-${version}.tar.gz";
    sha256 = "1rj9l8x0xyidj6bhlpwb9qfc9d70nkwgmn69k7cx4y9g9pz25cqs";
  };
  propagatedBuildInputs = with pythonPackages; [ numpy ];
  doCheck = false; # TODO can we do some of the tests only?
}
