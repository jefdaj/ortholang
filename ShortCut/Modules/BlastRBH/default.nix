# TODO pass proper arguments
# TODO shells for working on individual scripts (split by language?)

with import ../../../nixpkgs;

let
  # parallelblast = callPackage ./parallelblast {};
  # myPython = pkgs.pythonPackages.python.withPackages (ps: with ps; [
    # docopt
    # ]);
  myR = pkgs.rWrapper.override { packages = with pkgs.rPackages; [
    dplyr
    data_table
  ];};
  # runDepends = [ myR parallelblast ];
  # runDepends = [ parallel ncbi-blast myPython myR ];
  runDepends = [ myR ];

in stdenv.mkDerivation {
  name = "shortcut-blastrbh";
  src = ./.;
  inherit runDepends;
  buildInputs = [ makeWrapper ] ++ runDepends;
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    for script in $src/*; do
      base="$(basename "$script")"
      [[ "$base" == default.nix ]] && continue
      dest="$out/bin/$base"
      install -m755 $script $dest
      wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
}
