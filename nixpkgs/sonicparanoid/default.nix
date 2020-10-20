# with (import ./..);
{ pkgs, python3Packages, fetchurl, mmseqs2, mcl }:

let
  # this is only needed for sh >= 1.12.14; remove once nixpkgs includes it
  pypiPython = import ./requirements.nix { inherit pkgs; };

in python3Packages.buildPythonPackage rec {
  pname = "sonicparanoid";
  version = "1.3.4";

  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/90/ad/aadec7847f30cd1403e6a6f4c04caebdb3c3837a8641fa0befd59a9f8cbf/sonicparanoid-1.3.4.tar.gz";
    sha256 = "1wqzlc2zsl8yz1qi4xv2vqfa6bdpfwf2r04j496y1bbyw2g6v6ps";
  };

  propagatedBuildInputs = with python3Packages; [
    mmseqs2

    # python3Packages.biopython
    # python3Packages.numpy
    # python3Packages.pandas
    # python3Packages.sh
    # python3Packages.psutil
    # python3Packages.mypy

    # from install_requires in setup.py
    scipy
    numpy
    pandas
    cython
    setuptools
    pip
    biopython
    mypy
    psutil
    sklearn-deap # scikit-learn
    wheel

  ];

  buildInputs = [
    # not sure if useful:
    # cd-hit http://weizhongli-lab.org/cd-hit/
    # muscle
    # biopython
    # hmmer
    mcl
    # mmseqs2
    # numpy
    # pandas
    python3Packages.cython
    python3Packages.sh
    python3Packages.psutil
    python3Packages.mypy
  ];

  # TODO get the tests working! probably just need packages here?
  doCheck = false;
  # checkInputs = with pypiPython.packages; [];

  patches = [
    # TODO which of these are still needed? update them
    # ./add-nix-dependency-paths.patch
    # ./disable-version-checks.patch
    # ./ignore-divide-by-zero.patch # TODO is this a bad idea?
  ];

  # Once the patch above has replaced the setup code with Nix vars,
  # this fills in the full paths. Overall pretty hacky, but seems to work.
  inherit mcl mmseqs2;
  postPatch = ''
    for f in sonicparanoid/*.py*; do
      substituteAllInPlace "$f"
    done
  '';

  meta = {
    # TODO write this
  };
}
