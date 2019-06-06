{ stdenv
, buildPythonPackage
, fetchPypi
, cython
}:

buildPythonPackage rec {
  pname = "tree_distance";
  version = "1.0.6";
  name = "${pname}-${version}";
  src = fetchPypi {
    inherit pname version;
    sha256 = "02g98zaai70sw057chizm3fjwblzpsgsw2jba8xc8wnl3wsx9mjp";
  };
  buildInputs = [ cython ];
}
