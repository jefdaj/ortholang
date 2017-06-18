# TODO pass proper arguments
# TODO shells for working on individual scripts (split by language?)
# TODO other random unix dependencies: uname

with import ../nixpkgs;

let
  myRWrapper = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      dplyr
      biomartr # TODO is version 0.50 any better for now?
    ];
  };

in stdenv.mkDerivation {
  name = "shortcut-scripts";
  # TODO version?
  src = ./.;
  buildInputs = [
    makeWrapper
    pythonPackages.biopython
    myRWrapper
    shmlast
    last-align
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
      wrapProgram $script \
        --prefix PYTHONPATH : $(toPythonPath ${pythonPackages.biopython}) \
        --prefix PATH : "${myRWrapper}/bin" "${shmlast}/bin"
    done
  '';

  shellHook = ''
    export PATH=${myRWrapper}/bin:${pythonPackages.ipython}/bin:${shmlast}/bin:\$PATH
  '';
}
