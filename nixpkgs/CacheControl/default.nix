{ buildPythonPackage
, fetchPypi
, requests
, msgpack
, lockfile
, redis
}:

buildPythonPackage rec {
  pname = "CacheControl";
  version = "0.12.5";
  name = "${pname}-${version}";
  src = fetchPypi {
    inherit pname version;
    sha256 = "1xrgljm1awxvyf3k3yjkwanrhkvkwf3vglx2yrw33d2ivzzpxxyf";
  };
  buildInputs = [ ];
  propagatedBuildInputs = [ requests msgpack lockfile redis ];
}
