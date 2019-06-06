{ stdenv
, buildPythonPackage
, fetchPypi
, numpy
, scipy
}:

buildPythonPackage rec {
  pname = "fastcluster";
  version = "1.1.25";
  name = "${pname}-${version}";
  src = fetchPypi {
    inherit pname version;
    sha256 = "14dh0w56hzz53ja690s8v9ycnc64d42bz12cv4w1lm5y7za897ak";
  };
  buildInputs = [ numpy scipy ];
}
