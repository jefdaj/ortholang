with import ../../../nixpkgs;

let
  runDepends = [
    psiblast-exb # TODO is this still the best version to use?
    hmmer
    busco
    python36Packages.python # same version as busco for compatibility
  ];

in stdenv.mkDerivation {
  name = "shortcut-busco";
  src = ./.;
  inherit runDepends;
  buildInputs = [ makeWrapper ] ++ runDepends;
  # the substitution is to pick up the path to the busco config template
  inherit busco;
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    for script in $src/*; do
      base="$(basename "$script")"
      [[ "$base" == default.nix ]] && continue
      dest="$out/bin/$base"
      install -m755 $script $dest
      substituteAllInPlace $dest
      wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
}
