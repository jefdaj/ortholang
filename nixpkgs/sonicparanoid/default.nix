# with (import ./..);
{ pkgs, python3Packages, fetchurl, mmseqs2 }:

let
  # this is only needed for sh >= 1.12.14; remove once nixpkgs includes it
  pypiPython = import ./requirements.nix { inherit pkgs; };

in python3Packages.buildPythonPackage rec {
  pname = "sonicparanoid";
  version = "1.0.14";

  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/a2/29/deabc1920ab6c59fa199b848ab8a0956c1b6612070842d5ce5174320b7bc/${pname}-${version}.tar.gz";
    sha256 = "0nvnvgjc45y7rdfvlmgwyc5xv3xv3lmxcny1z1ilj1hwdjlh1dbx";
  };

  propagatedBuildInputs = [
    mmseqs2
    python3Packages.biopython
    python3Packages.numpy
    python3Packages.pandas
    pypiPython.packages.sh
  ];

  buildInputs = [
    # not sure if useful:
    # cd-hit http://weizhongli-lab.org/cd-hit/
    # muscle
    # biopython
    # hmmer
    # mcl
    # mmseqs2
    # numpy
    # pandas
    python3Packages.cython
    pypiPython.packages.sh
  ];

  # TODO get the tests working! probably just need packages here?
  doCheck = false;
  # checkInputs = with pypiPython.packages; [];

  patches = [
    ./find-mmseqs-bin.patch
    ./allow-new-mmseqs.patch
  ];

  meta = {
    # TODO write this
  };
}
