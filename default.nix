# This imports a bunch of random stuff that I ended up having to package,
# which will hopefully be merged into upstream nixpkgs before publication.
with import ./nixpkgs;
let

  # This imports the Haskell build info, which is in a separate file so it can
  # be auto-updated. To do that, run `cabal2nix ./. > shortcut.nix`
  shortcut = haskellPackages.callPackage ./shortcut.nix {};

  # List R packages here
  myR = pkgs.rWrapper.override { packages = with pkgs.rPackages; [
    dplyr
    biomartr # TODO is version 0.50 any better for now?
  ];};

  # List Python packages here
  myPython = pkgs.pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);

  # List packages needed at runtime here
  runDepends = [
    shmlast
    ncbi-blast # TODO try the newest version
    crb-blast  # TODO try with the newest ncbi-blast
    coreutils
    last-align
    myR
    myPython
  ];

  # List packages you use during development here
  # (It's also be fine to remove what you don't want, for example vim)
  shellDepends = runDepends ++ [
    stdenv # bunch of standard unix programs
    stack # TODO stack in nix-shell, or stack with nix-managed dependenencies?
    which
    xz
    gnumake
    gnutar
  ];

in stdenv.mkDerivation {
  # TODO decide on a version scheme
  name = "shortcut";
  src = ./.;
  buildInputs = [ makeWrapper ];

  # This generates an overall wrapper script called `shortcut` which has all
  # required dependencies on its PATH.
  # TODO why does this work when the haskell overrideDerivation style doesn't?
  #      (see https://github.com/jml/nix-haskell-example)
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    wrapperScript=$out/bin/shortcut
    cat << EOF > $wrapperScript
    #!/usr/bin/env bash
    ${shortcut}/bin/shortcut \$@
    EOF
    chmod +x $wrapperScript
    wrapProgram $wrapperScript --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';

  # Commands here are run on entering the development shell
  shellHook = ''
    export PATH="${pkgs.lib.makeBinPath shellDepends}:\$PATH"
  '';
}
