{fetchurl, python3Packages, parallel, last-align, coreutils}:
# with import ../.;
with python3Packages;

let
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
    name = "ficus-0.3.2";
    src = fetchurl {
      url = "https://github.com/camillescott/ficus/archive/v0.3.2.tar.gz";
      sha256 = "14yllgdssqdkjby0923yv8p3mdjv9639d6q2zfdk1c9yjq3i6lsk";
    };
    patchPhase = ''
      sed -i  '/nose-capturestderr.*/d'  requirements.txt
      sed -i "s/'nose-capturestderr'//g" setup.py
    '';
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

# TODO need lastdb, lastal at runtime

in buildPythonPackage rec {
  name = "shmlast-${version}";
  version = "1.1";
  src = fetchurl {
    url = "https://github.com/camillescott/shmlast/archive/v1.1.tar.gz";
    sha256 = "0654b2fdark0x1vagkqmydg5193vgnsjby95hnvl3a5m2a3zz2qd";
  };
  patchPhase = ''
    sed -i 's/matplotlib==.*/matplotlib/g' requirements.txt
    sed -i "s/'matplotlib.=.*'/'matplotlib'/g" setup.py
    sed -i "s/'pytest.=.*'/'pytest'/g"         setup.py
    sed -i 's/long_description.*/long_description = "unicode error reading README.rst",/g' setup.py
  '';
  doCheck = false;
  buildInputs = [
    # doit
    # ficus
    # filelock
    # last-align
    # matplotlib
    # parallel
    # seaborn
    pytest
    scipy
  ];
  propagatedBuildInputs = [
    pandas
    matplotlib
    doit
    last-align # TODO add to scripts dependencies too?
    parallel
    ficus
    seaborn
    numpy
    filelock
    screed
    coreutils # for uname
    # TODO only for shell:
    # which
  ];
}
