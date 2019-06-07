# with (import ./..);
{ pkgs, python36Packages, fetchurl, mmseqs2, mcl }:

let
  # this is only needed for sh >= 1.12.14; remove once nixpkgs includes it
  pypiPython = import ./requirements.nix { inherit pkgs; };

in python36Packages.buildPythonPackage rec {
  pname = "sonicparanoid";
  version = "1.2.2";

  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/89/a7/8d12cb5ee5f4443db9da8d7a7b34becb8b4e947ae9201978ea1d76c18056/sonicparanoid-1.2.2.tar.gz";
    sha256 = "1wmnkcmn721xwg9ry43z8ncl5dfwyyi6mh1nxd5rd0vck8ph7mfk";
  };

  propagatedBuildInputs = [
    mmseqs2
    python36Packages.biopython
    python36Packages.numpy
    python36Packages.pandas
    pypiPython.packages.sh
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
    pypiPython.packages.sh
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
