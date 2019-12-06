{ buildPythonPackage
, fetchPypi
, cython
, numpy
, dendropy
}:

buildPythonPackage rec {
  pname = "phylo_utils";
  version = "0.0.5";
  name = "${pname}-${version}";
  src = fetchPypi {
    inherit pname version;
    sha256 = "1qsihakwrh47pmpbwzy62z1z3wn0f5bjw7kv5py28a26q88aip3y";
  };
  buildInputs = [ cython ];
  propagatedBuildInputs = [ numpy dendropy ];
}
