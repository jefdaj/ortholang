# TODO: finish this if shmlast works well

# {fetchurl, fetchgit, python3Packages, parallel, last-align}:
with import <nixpkgs> {};
with python3Packages;

let
  last-align = callPackage ../last-align {}; # TODO remove when calling as a fn
  doit = buildPythonPackage {
    name = "doit-0.30.3";
    src = fetchurl {
      url = "https://pypi.python.org/packages/bf/8a/9941b2fe2d0c8c22fe619281b38d132250d65f7a4a6a54463f7fedf62657/doit-0.30.3.tar.gz";
      sha256 = "1fcsslc3mc4bszq5xdqbxv37720s1s31d6pbfwc2iyxk1x2wi219";
    };
    buildInputs = [pyinotify cloudpickle pytest mock];
    propagatedBuildInputs = [cloudpickle pyinotify];
  };
  filelock = buildPythonPackage {
    name = "filelock-2.0.8";
    src = fetchurl {
      url = "https://pypi.python.org/packages/55/fb/ad353636e03b66bc60c57e0e5e3e196bfdc08a030e5e16885da7cddf1bc0/filelock-2.0.8.tar.gz";
      sha256 = "0ar16p4ibjcc66pmxmizlvpxyl0hmsrmw8wg9pbabjg3dn8f8j3y";
    };
  };
  bz2file = buildPythonPackage {
    name = "bz2file-0.98";
    src = fetchurl {
      url = "https://pypi.python.org/packages/61/39/122222b5e85cd41c391b68a99ee296584b2a2d1d233e7ee32b4532384f2d/bz2file-0.98.tar.gz";
      sha256 = "126s53fkpx04f33a829yqqk8fj4png3qwg4m66cvlmhmwc8zihb4";
    };
    doCheck = false;
  };
  ficus = buildPythonPackage {
    name = "ficus-0.3";
    # TODO seems to work as long as you remove the nosetest requirement?
    # src = fetchurl {
    #   url = "https://pypi.python.org/packages/1a/ec/b92090badf7093ab2a329f2be6e7ded0803f7dfe0b8196bc3881a53b19b2/ficus-0.3.tar.gz";
    #   sha256 = "0q1mpzr5iwys7y754lnzfp812jhl3qa19lp79fb2f0ji3ny1ckh0";
    # };
    src = ./ficus;
    buildInputs = [matplotlib];
    doCheck = false;
    # propagatedBuildInputs = [bz2file];
  };
  screed = buildPythonPackage {
    name = "screed-1.0";
    src = fetchurl {
      url = "https://pypi.python.org/packages/e7/7a/b5f16a9861ac497b346b2f5205e8dc35103eeba1e6fdffa114c47e0b35ba/screed-1.0.tar.gz";
      sha256 = "148vcb7w2wr6a4w6vs2bsxanbqibxfk490zbcbg4m61s8669zdjx";
    };
    buildInputs = [bz2file];
    doCheck = false;
    propagatedBuildInputs = [bz2file];
  };

in buildPythonPackage rec {
  name = "shmlast-${version}";
  version = "1.1";
  # src = fetchgit {
  #   url = "https://github.com/camillescott/shmlast";
  #   rev = "1819885e67f37875450c87556b694b54803e4538";
  #   sha256 = "11kmddbkdki87296gil63zd6rd7nms9nxlvwvc4wmls18254bifb";
  # };
  src = ./shmlast;
  buildInputs = [
    matplotlib
    parallel
    last-align
    doit
    filelock
    scipy
    seaborn
    screed
    ficus
  ];
  doCheck = false;
  propagatedBuildInputs = [
    pandas
    matplotlib
    doit
    last-align
    parallel
    ficus
    seaborn
    numpy
  ]; # TODO remove?
  # patchPhase = "sed -i 's/long-description/\#long-description/g' $src/setup.py";
}
