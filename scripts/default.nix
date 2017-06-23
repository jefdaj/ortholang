# TODO pass proper arguments
# TODO shells for working on individual scripts (split by language?)

with import ../nixpkgs;

let
  myR = pkgs.rWrapper.override { packages = with pkgs.rPackages; [
    dplyr
    biomartr # TODO is version 0.50 any better for now?
  ];};
  myPython = pkgs.pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);
  runDepends = [
    # shmlast # TODO remove along with the haskell module
    # last-align # TODO remove here and add to crb-blast
    # coreutils # TODO remove?
    myR
    myPython
  ];

in stdenv.mkDerivation rec {
  name = "shortcut-scripts"; # TODO version?
  src = ./.;
  buildInputs = [ makeWrapper ];
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    for script in $src/*.{py,R}; do
      install -m755 $script $out/bin
      base="$(basename "$script")"
      wrapProgram $out/bin/$base \
        --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
  shellHook = ''
    export PATH="${pkgs.lib.makeBinPath runDepends}:\$PATH"
  '';
}
