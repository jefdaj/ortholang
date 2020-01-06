with import ./nixpkgs;
let
  # from nixpkgs/pkgs/applications/networking/cluster/mesos/default.nix
  tarWithGzip = lib.overrideDerivation gnutar (oldAttrs: {
    builder = "${bash}/bin/bash";
    buildInputs = (oldAttrs.buildInputs or []) ++ [ makeWrapper ];
    postInstall = (oldAttrs.postInstall or "") + ''
      wrapProgram $out/bin/tar --prefix PATH ":" "${gzip}/bin"
    '';
  });

  # fixes "Fontconfig error: Cannot load default config file"
  # from nixpkgs/pkgs/development/libraries/pipewire/default.nix
  fontsConf = makeFontsConf {
    fontDirectories = [ ];
  };

  myEnv = [
    # TODO which of these are needed?
    stdenv
    bash
    bashInteractive
    coreutils
    diffutils
    glibcLocales # TODO even on mac?
    tree
    tarWithGzip
    gnugrep
    gnused
    gawk
    curl
    cacert # for curl https
    fontconfig.lib # for R plotting scripts
  ];

  # TODO why is patching shebangs on the wrapped scripts necessary??
  mkModule = src: extraRunDeps: extraWraps:
    let name = "Shortcut-" + baseNameOf src;
        runDeps = lib.lists.unique (myEnv ++ extraRunDeps);
    in stdenv.mkDerivation {
      inherit src name extraRunDeps extraWraps;
      NIX_LDFLAGS = "-lfontconfig"; # for R plotting scripts
      buildInputs = [ makeWrapper ] ++ runDeps;
      installPhase = ''
        source ${stdenv}/setup
        mkdir -p $out/bin
        for script in $src/*; do
          base="$(basename "$script")"
          dest="$out/bin/$base"
          substituteAll $script $dest
          chmod +x $dest
          wrapProgram $dest \
            --prefix PATH : "${pkgs.lib.makeBinPath runDeps}" ${extraWraps} \
            --set NIX_SSL_CERT_FILE "${cacert}/etc/ssl/certs/ca-bundle.crt" \
            --set FONTCONFIG_FILE "${fontsConf}"
        done
      '';

      # problem:  https://github.com/NixOS/nixpkgs/issues/11133
      # solution: https://github.com/NixOS/nixpkgs/pull/74942
      fixupPhase = if stdenv.isDarwin then ''
        for script in $out/bin/.*-wrapped; do
          patchShebangs "$script"
          substituteInPlace $script --replace "#!/nix" "#!/usr/bin/env /nix"
        done
      '' else ''
        for script in $out/bin/.*-wrapped; do
          patchShebangs "$script"
        done
      '';
    };

in rec {
  myBlast = ncbi-blast; # for swapping it out if needed

  # TODO remove these in favor of buildPythonPath!
  myPy2Wrap = "--prefix PYTHONPATH : \"$out/bin:${myPy2.python.sitePackages}\"";
  myPy3Wrap = "--prefix PYTHONPATH : \"$out/bin:${python36.python.sitePackages}\"";

  myPy2 = python27.buildEnv.override {

    # see https://github.com/NixOS/nixpkgs/issues/22319
    ignoreCollisions = true;

    extraLibs = with python27Packages; [
      biopython
      numpy
      scipy
    ];
  };

  myR = rWrapper.override { packages = with rPackages; [
    dplyr
    ggplot2
    readr
    tidyr
    UpSetR
    VennDiagram
    biomartr
    data_table
    futile_logger
  ];};

  shortcut-biomartr      = mkModule ./ShortCut/Modules/BioMartR      [ myR ] "";
  shortcut-blasthits     = mkModule ./ShortCut/Modules/BlastHits     [ myR ] "";
  shortcut-blastrbh      = mkModule ./ShortCut/Modules/BlastRBH      [ myR ] "";
  shortcut-plots         = mkModule ./ShortCut/Modules/Plots         [ myR ] "";
  shortcut-setstable     = mkModule ./ShortCut/Modules/SetsTable     [ myR ] "";
  shortcut-range         = mkModule ./ShortCut/Modules/Range         [ myR ] "";
  shortcut-blast         = mkModule ./ShortCut/Modules/Blast         [ myBlast ] "";
  shortcut-blastdb       = mkModule ./ShortCut/Modules/BlastDB       [ myBlast blastdbget ] "";
  shortcut-crbblast      = mkModule ./ShortCut/Modules/CRBBlast      [ crb-blast ] "";
  shortcut-diamond       = mkModule ./ShortCut/Modules/Diamond       [ diamond ] "";
  shortcut-hmmer         = mkModule ./ShortCut/Modules/Hmmer         [ myPy2 hmmer ] myPy2Wrap;
  shortcut-mmseqs        = mkModule ./ShortCut/Modules/MMSeqs        [ mmseqs2 ] "";
  shortcut-muscle        = mkModule ./ShortCut/Modules/Muscle        [ muscle ] "";
  shortcut-psiblast      = mkModule ./ShortCut/Modules/PsiBlast      [ myBlast ] "";

  # TODO should the wrap not be necessary?
  shortcut-seqio         = mkModule ./ShortCut/Modules/SeqIO         [ myPy2 ] myPy2Wrap;
  shortcut-orthofinder   = mkModule ./ShortCut/Modules/OrthoFinder   [ myPy2 myBlast diamond orthofinder mcl fastme ] myPy2Wrap;
  shortcut-sonicparanoid = mkModule ./ShortCut/Modules/SonicParanoid [ sonicparanoid ] myPy3Wrap;

  shortcut-treecl        = mkModule ./ShortCut/Modules/TreeCl        [ myPy2 treeCl ] myPy2Wrap;
  # shortcut-justorthologs = mkModule ./ShortCut/Modules/JustOrthologs [ justorthologs ] "";

  # this config file is only a template; it needs to be completed by busco.sh at runtime
  shortcut-busco = mkModule ./ShortCut/Modules/Busco
                     [ myBlast hmmer busco python36 which tarWithGzip ]
                     "--set BUSCO_CONFIG_FILE ${busco}/config/config.ini";

  shortcut-load          = mkModule ./ShortCut/Modules/Load          [ curl ] "";
  shortcut-orthogroups   = mkModule ./ShortCut/Modules/OrthoGroups   [ python36 ] "";
  shortcut-greencut      = mkModule ./ShortCut/Modules/GreenCut      [ myPy2 ] myPy2Wrap;

  modules = [
    shortcut-biomartr
    shortcut-blast
    shortcut-blastdb
    shortcut-blasthits
    shortcut-blastrbh
    shortcut-crbblast
    shortcut-diamond
    shortcut-hmmer
    shortcut-mmseqs
    shortcut-muscle
    shortcut-orthofinder
    shortcut-plots
    shortcut-setstable
    shortcut-psiblast
    shortcut-seqio
    # shortcut-sonicparanoid
    # shortcut-treecl
    # shortcut-justorthologs
    shortcut-busco
    shortcut-load
    shortcut-range
    shortcut-orthogroups
    shortcut-greencut
  ];

  runDepends = modules ++ myEnv;
  # TODO separate list of useful, non-conflicting programs to put on PATH?
}
