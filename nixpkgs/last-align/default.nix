{stdenv, fetchzip, rsync}:

stdenv.mkDerivation {
  name = "last-align-861";
  src = fetchzip {
    url = "http://last.cbrc.jp/last-861.zip";
    sha256 = "1b3j7sn588r528hdappmnps6pqc10qg6q0lasxdllza2z9fyzlh0";
  };
  buildInputs = [ rsync ];
  buildPhase = ''
    cd $src
    make install prefix=$out
  '';
}
