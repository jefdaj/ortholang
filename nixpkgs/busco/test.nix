with import ./..;
let busco = import ./default.nix;
in python36Packages.callPackage busco {
  inherit makeWrapper;
  inherit hmmer psiblast-exb;
  inherit rPackages rWrapper;
  inherit (pkgs.lib) makeBinPath;
}
