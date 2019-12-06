{ buildPythonPackage
, fetchgit
, makeWrapper
, ncbi-blast
, hmmer
, rPackages
, rWrapper
, makeBinPath
, substituteAll
# TODO augustus?
}:

let
  # these are required for generate_plot.py which calls Rscript
  myR = rWrapper.override { packages = with rPackages; [
    ggplot2
    gridBase # TODO gridExtra?
  ];};

  # for some reason substituteAll doesn't like names with dashes
  # also now we can change to NCBI BLAST+ or another one
  myBlast = ncbi-blast;

  runDepends = [
    myR
    hmmer
    myBlast
  ];

in buildPythonPackage {
  name = "busco";
  version = "3.0.0"; # is it actually 3.0.2 now?
  src = fetchgit {
    url = "https://gitlab.com/ezlab/busco";
    rev = "e83a6c94101511484799f9770cdfc148559b136d";
    sha256 = "0ivc028gdryyvpznkzw2pbm5pj1x6svdawdr7gmbrdm6111gnsgc";
  };

  # TODO which ones should go in which of these and which in runDepends?
  buildInputs = [
    makeWrapper
    myR
    hmmer
    myBlast
  ];
  propatagedBuildInputs = [ ];

  patches = [
    (substituteAll {
      src = ./add-nix-bin-paths.patch;
      inherit myR hmmer myBlast;
    })
  ];

  postInstall = ''
    mkdir -p $out/bin
    cp $src/scripts/* $out/bin

    # this will be edited further at runtime for each fn call
    mkdir -p $out/config
    cp config/config.ini.default $out/config/config.ini

    # if not editing further, would also --set BUSCO_CONFIG_FILE here
    for py in $out/bin/*; do
      wrapProgram "$py" --prefix PATH : "${makeBinPath runDepends}"
    done
  '';
}
