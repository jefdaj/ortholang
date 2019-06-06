{ buildPythonPackage
, fetchPypi
}:

buildPythonPackage rec {
  pname = "progressbar-latest";
  version = "2.4";
  name = "${pname}-${version}";
  src = fetchPypi {
    inherit pname version;
    sha256 = "0q50nyfpya11v335csbmnbrfmrw3424v5z7ysx0jagsm0khlc2cj";
  };
  buildInputs = [];
}
