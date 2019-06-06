{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "fasttree-${version}";
  version = "2.1.9";
  src = fetchurl {
    url = "http://microbesonline.org/fasttree/FastTree-${version}.c";
    sha256 = "0ljvvw8i1als1wbfzvrf15c3ii2vw9db20a259g6pzg34xyyb97k";
  };
  builder = ./builder.sh;
}
