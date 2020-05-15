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
    fsatrace # for shake linting
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
    let name = "OrthoLang-" + baseNameOf src;
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
          patchShebangs $script
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

  ortholang-biomartr      = mkModule ./OrthoLang/Modules/BioMartR      [ myR ] "";
  ortholang-blasthits     = mkModule ./OrthoLang/Modules/BlastHits     [ myR ] "";
  ortholang-blastrbh      = mkModule ./OrthoLang/Modules/BlastRBH      [ myR ] "";
  ortholang-plots         = mkModule ./OrthoLang/Modules/Plots         [ myR ] "";
  ortholang-setstable     = mkModule ./OrthoLang/Modules/SetsTable     [ myR ] "";
  ortholang-range         = mkModule ./OrthoLang/Modules/Range         [ myR ] "";
  ortholang-blast         = mkModule ./OrthoLang/Modules/Blast         [ myBlast parallel ] "";
  ortholang-blastdb       = mkModule ./OrthoLang/Modules/BlastDB       [ myBlast blastdbget ] "";
  ortholang-crbblast      = mkModule ./OrthoLang/Modules/CRBBlast      [ crb-blast ] "";
  ortholang-depgraph      = mkModule ./OrthoLang/Modules/DepGraph      [ graphviz ] "";
  ortholang-diamond       = mkModule ./OrthoLang/Modules/Diamond       [ diamond ] "";
  ortholang-hmmer         = mkModule ./OrthoLang/Modules/Hmmer         [ myPy2 hmmer ] myPy2Wrap;
  ortholang-mmseqs        = mkModule ./OrthoLang/Modules/MMSeqs        [ mmseqs2 ] "";
  ortholang-muscle        = mkModule ./OrthoLang/Modules/Muscle        [ muscle ] "";
  ortholang-psiblast      = mkModule ./OrthoLang/Modules/PsiBlast      [ myBlast ] "";

  # TODO should the wrap not be necessary?
  ortholang-seqio         = mkModule ./OrthoLang/Modules/SeqIO         [ myPy2 ] myPy2Wrap;
  ortholang-orthofinder   = mkModule ./OrthoLang/Modules/OrthoFinder   [ myPy2 myBlast diamond orthofinder mcl fastme ] myPy2Wrap;
  ortholang-sonicparanoid = mkModule ./OrthoLang/Modules/SonicParanoid [ sonicparanoid ] myPy3Wrap;

  ortholang-treecl        = mkModule ./OrthoLang/Modules/TreeCl        [ myPy2 treeCl ] myPy2Wrap;
  # ortholang-justorthologs = mkModule ./OrthoLang/Modules/JustOrthologs [ justorthologs ] "";

  # this config file is only a template; it needs to be completed by busco.sh at runtime
  ortholang-busco = mkModule ./OrthoLang/Modules/Busco
                     [ myBlast hmmer busco python36 which tarWithGzip ]
                     "--set BUSCO_CONFIG_FILE ${busco}/config/config.ini";

  ortholang-curl          = mkModule ./OrthoLang/Modules/Curl          [ curl ] "";
  ortholang-load          = mkModule ./OrthoLang/Modules/Load          [ ] "";
  ortholang-orthogroups   = mkModule ./OrthoLang/Modules/OrthoGroups   [ python36 ] "";
  ortholang-greencut      = mkModule ./OrthoLang/Modules/GreenCut      [ myPy2 ] myPy2Wrap;

  modules = [
    ortholang-biomartr
    ortholang-blast
    ortholang-blastdb
    ortholang-blasthits
    ortholang-blastrbh
    ortholang-crbblast
    ortholang-depgraph
    ortholang-diamond
    ortholang-hmmer
    ortholang-mmseqs
    ortholang-muscle
    ortholang-orthofinder
    ortholang-plots
    ortholang-setstable
    ortholang-psiblast
    ortholang-seqio
    # ortholang-sonicparanoid
    # ortholang-treecl
    # ortholang-justorthologs
    ortholang-busco
    ortholang-curl
    ortholang-load
    ortholang-range
    ortholang-orthogroups
    ortholang-greencut
  ];

  runDepends = modules ++ myEnv;
  # TODO separate list of useful, non-conflicting programs to put on PATH?
}
