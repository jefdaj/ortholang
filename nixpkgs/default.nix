# Maintainers willing, everything here is destined to be merged into
# the main upstream repository at https://github.com/nixos/nixpkgs

let
  # fetch my pinned nixpkgs and build everything using that for reproducibility.
  # note that it might take a really long time on your system.
  # use this instead to try to build it with your system's current nixpkgs:
  # pkgs = import <nixpkgs> {};
  # to update the the sha256sum, use nix-prefetch-url --unpack
  # (see https://github.com/NixOS/nix/issues/1381#issuecomment-300755992)
  pkgs = import (fetchTarball {
    # url = "https://github.com/jefdaj/nixpkgs/archive/2018-12-02_shortcut-demo.tar.gz";
    # sha256 = "1s9q5r4rar0j5xsyr6d7cr09p98pxx3lbnhryk8ch5qy1h0klp4h";
    url = "https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz";
    sha256 = "043npvvr8zxndhq6mfyzriv3b363biy2cnskpngv9fkxmizszrfl";
  }) {};

  psiblast-exb = pkgs.callPackage ./psiblast-exb { };
  hmmer = pkgs.callPackage ./hmmer { };

  # ... because it only supports exactly 2.2.29
  # and there are reports of a bug in newer ones (still?)
  # TODO patch crb-blast to use the newest one?
  ncbi-blast = (pkgs.callPackage ./ncbi-blast {}).overrideDerivation (old: rec {
    version="2.2.29";
    name="ncbi-blast-${version}";
    src = pkgs.fetchurl {
      url = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/${version}/ncbi-blast-${version}+-x64-linux.tar.gz";
      sha256="1pzy0ylkqlbj40mywz358ia0nq9niwqnmxxzrs1jak22zym9fgpm";
    };
  });
  crb-blast  = pkgs.callPackage ./crb-blast  { inherit ncbi-blast; };

  # cdhit = pkgs.callPackage ./cdhit { };
  mcl = pkgs.callPackage ./mcl { };
  fastme = pkgs.callPackage ./fastme { };
  # muscle = pkgs.callPackage ./muscle { }; # TODO got this already somewhere!
  diamond = pkgs.callPackage ./diamond { };
  mmseqs2 = pkgs.callPackage ./mmseqs2 { };
  orthofinder = pkgs.callPackage ./orthofinder { inherit mcl fastme psiblast-exb diamond; };

  # TODO push new sh-1.12.14 upstream! haven't managed to include it properly here
  sonicparanoid = pkgs.callPackage ./sonicparanoid {
    inherit mmseqs2;
    # python3Packages = myPython3;
  };

  myPython = pkgs.pythonPackages // {
    blastdbget = pkgs.callPackage ./blastdbget {};
    biopython  = pkgs.callPackage ./biopython {
      inherit pkgs;
      inherit (pkgs) pythonPackages;
    };
  };

#   myPython3 = pkgs.python3Packages // {
#     sh = pkgs.callPackage ./sh {};
#   };

in pkgs // {
  inherit ncbi-blast crb-blast psiblast-exb diamond hmmer orthofinder mmseqs2 sonicparanoid;
  pythonPackages = myPython;
  # python3Packages = myPython3;
}
