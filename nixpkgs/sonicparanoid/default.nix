with import <nixpkgs> {};
let
  diamond = callPackage ../diamond {};
  mcl = callPackage ../mcl {};
  # biopython = callPackage ../biopython {};
  mmseqs2 = callPackage ../mmseqs2 {};
  hmmer = callPackage ../hmmer {};
  p =

# TODO muscle?
# TODO is python3Packages not necessary? or should everything be done through it?
{ mcl, diamond, hmmer, mmseqs2, python3Packages }:
with python3Packages;

buildPythonPackage rec {
  pname = "sonicparanoid";
  version = "1.0.14";

  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/a2/29/deabc1920ab6c59fa199b848ab8a0956c1b6612070842d5ce5174320b7bc/${pname}-${version}.tar.gz";
    sha256 = "0nvnvgjc45y7rdfvlmgwyc5xv3xv3lmxcny1z1ilj1hwdjlh1dbx";
  };

  # TODO just regular buildInputs? or wrap the exe?
  propagatedBuildInputs = with python3Packages; [
    sh # TODO add updated version to this repo + upstream
    numpy
    cython
    pandas
    biopython
    mmseqs2
  ];
  buildInputs = [
    numpy
    cython
    pandas
    biopython
    diamond # TODO remove?
    hmmer # TODO remove?
    mcl # TODO remove?
    # mmseqs2
    # TODO muscle 
    # TODO package cd-hit? http://weizhongli-lab.org/cd-hit/
  ];

  # TODO get the tests working! issue with not loading cython files?
  doCheck = false;

  patches = [
    ./find-mmseqs-bin.patch
  ];

  meta = {
    # description = "Python library for bioinformatics";
    # longDescription = ''
    #   Biopython is a set of freely available tools for biological computation
    #   written in Python by an international team of developers. It is a
    #   distributed collaborative effort to develop Python libraries and
    #   applications which address the needs of current and future work in
    #   bioinformatics.
    # '';
    # homepage = http://biopython.org/wiki/Documentation;
    # maintainers = with lib.maintainers; [ luispedro ];
    # license = lib.licenses.bsd3;
  };
}

; in callPackage p { inherit diamond mcl hmmer mmseqs2; }
