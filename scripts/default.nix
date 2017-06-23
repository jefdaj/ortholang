# TODO pass proper arguments
# TODO shells for working on individual scripts (split by language?)

with import ../nixpkgs;

let
  myR = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      dplyr
      biomartr # TODO is version 0.50 any better for now?
    ];
  };
  myPython = pkgs.pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);

in stdenv.mkDerivation rec {
  # TODO version?
  name = "shortcut-scripts";
  src = ./.;
  buildInputs = [ makeWrapper ];
  runDepends = [
    shmlast
    ncbi-blast
    crb-blast
    coreutils
    last-align
    myR
    myPython
  ];
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    for script in $src/*.{py,R}; do
      echo "$script"
      base="$(basename "$script")"
      install -m755 $script $out/bin
    done
    for script in $out/bin/*; do
      wrapProgram $script --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
  shellHook = ''
    export PATH="${pkgs.lib.makeBinPath runDepends}:\$PATH"
  '';
}
