# with (import ./..);
{ pkgs, python36Packages, fetchurl, mmseqs2, mcl }:

let
  # this is only needed for sh >= 1.12.14; remove once nixpkgs includes it
  pypiPython = import ./requirements.nix { inherit pkgs; };

in python36Packages.buildPythonPackage rec {
  pname = "sonicparanoid";
  version = "1.2.3";

  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/8d/bf/4337a0707bffc0cbf805f6d9580f0ca16a21213725d240b0292e6ea70523/sonicparanoid-1.2.3.tar.gz";
    sha256 = "1sgfg3qf3sj8sjlkiqin6cjp8b38rc58js361yb0ikzmy4nh8f06";
  };

  propagatedBuildInputs = [
    mmseqs2
    python36Packages.biopython
    python36Packages.numpy
    python36Packages.pandas
    python36Packages.sh
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
    python36Packages.cython
    python36Packages.sh
  ];

  # TODO get the tests working! probably just need packages here?
  doCheck = false;
  # checkInputs = with pypiPython.packages; [];

  patches = [
    ./add-nix-dependency-paths.patch
    ./disable-version-checks.patch
    ./ignore-divide-by-zero.patch # TODO is this a bad idea?
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
