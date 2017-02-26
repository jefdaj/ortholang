# TODO pass proper arguments

with import <nixpkgs> {};

let
  # TODO replace with lots of smaller scripts
  # TODO this isn't a dependency of the scripts, but shortcut itself right now!
  # bblast = import ./bblast;

  myR = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      dplyr
    ];
  };

in stdenv.mkDerivation {
  name = "shortcut-scripts";
  src = ./.;
  buildInputs = [
    myR
    # bblast
    pythonPackages.biopython
    makeWrapper
  ];
  # inherit bblast;
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
      wrapProgram $script \
        --prefix PYTHONPATH : $(toPythonPath ${pythonPackages.biopython}) \
        --prefix PATH : "${myR}/bin"
    done
  '';

  shellHook = ''
    export PATH=${myR}/bin:${pythonPackages.ipython}/bin:\$PATH
  '';
}
