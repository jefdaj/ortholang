# already in nixpkgs
{ fetchurl
, makeWrapper
, pyyaml
, biopython
, cython
, dendropy
, futures
, ipython
, matplotlib
, nose
, numpy
, pandas
, progressbar
, scikitlearn
, scipy

# packaged with shortcut
, fastcluster
, fasttree
, tree_distance
, progressbar-latest
, CacheControl
, scikit-bio
, phylo_utils

# edited version packaged with shortcut
, raxml

}:

let
  # TODO remove this and put all dependencies
  # pkgs = import /home/jefdaj/shortcut/nixpkgs;

  # TODO can this work without the pypi2nix requirements.nix stuff?
  #      it doesn't really use it much
  # python = import ./requirements.nix { inherit pkgs; };
  python = import ./requirements.nix { };

  # TODO move these up a level
  # fastcluster        = pkgs.python27Packages.callPackage ../fastcluster {};
  # fasttree           = pkgs.python27Packages.callPackage ../fasttree {};
  # tree_distance      = pkgs.python27Packages.callPackage ../tree_distance {};
  # progressbar-latest = pkgs.python27Packages.callPackage ../progressbar-latest {};
  # CacheControl       = pkgs.python27Packages.callPackage ../CacheControl {};
  # scikit-bio         = pkgs.python27Packages.callPackage ../scikit-bio { inherit CacheControl; };
  # phylo_utils        = pkgs.python27Packages.callPackage ../phylo_utils {};

in python.mkDerivation {
  name = "treecl-0.1.36";
  # src = ./.;
  src = fetchurl {
    url = "https://github.com/kgori/treeCl/archive/0.1.36.tar.gz";
    sha256 = "0y8lsl6fx4psd6if0kn81qaslnq0rpmr3jmr5xdipmk2rn97k9wn";
  };
  buildInputs = [
    makeWrapper
  ];
  propagatedBuildInputs = [

    # already in nixpkgs
    pyyaml
    biopython
    cython
    dendropy
    futures
    # ipython
    matplotlib
    nose
    numpy
    pandas
    progressbar
    scikitlearn
    scipy

    # packaged here
    fastcluster
    fasttree
    tree_distance
    progressbar-latest
    CacheControl
    scikit-bio
    phylo_utils

    # updated here
    # TODO be intelligent about whether we can use MPI on a given computer
    # TODO add an AVX(2) target (didn't I do that already?)
    raxml
   ];

  # TODO remove?
  preConfigure = ''
    export PATH=${raxml}/bin:$PATH
  '';

  # TODO patch to use MPI before AVX or regular?
  patches = [
    ./uncomment-binary.patch # TODO check with the author that this is OK
  ];

  # TODO remove now that it propagates?
  # TODO remove if preConfigure handles it
  # TODO do the non-python dependencies all need to go here too?
  # postInstall = ''
  #   for b in $out/bin/*; do
  #     wrapProgram $b --prefix PATH : "${raxml}/bin"
  #   done
  # '';
}
