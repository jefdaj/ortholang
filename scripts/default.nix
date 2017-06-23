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
      dest="$out/bin/$(basename "$script")"
      install -m755 $script $dest
      wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
  # TODO remove? buildInputs seems more effective
  shellHook = ''
    export PATH="${pkgs.lib.makeBinPath runDepends}:\$PATH"
  '';
}
