{ pythonPackages, fetchurl }:
with pythonPackages;

# TODO update this upstream

buildPythonPackage rec {
  name = "sh-1.12.14";

  src = fetchurl {
    url = "mirror://pypi/s/sh/${name}.tar.gz";
    sha256 = "b52bf5833ed01c7b5c5fb73a7f71b3d98d48e9b9b8764236237bdc7ecae850fc";
  };

  doCheck = false;

  meta = {
    description = "Python subprocess interface";
    homepage = https://pypi.python.org/pypi/sh/;
  };
}
