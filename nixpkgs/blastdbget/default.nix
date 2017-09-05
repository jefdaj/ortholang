{ pythonPackages, fetchgit }:
with pythonPackages;

# TODO patch to use the shortcut tmpdir for downloads rather than /tmp?

buildPythonPackage {
  name = "blastdbget";
  # version = "98ba28";
  prefix = "";
  src = fetchgit {
    url = "https://github.com/brwnj/blastdbget";
    rev = "98ba2871cb3ceb29b722926351374e1eae45cb75";
    sha256 = "0kfhqf03b52v5bbyi8rjx4lp1ncgvmy5fbzdbvgjnf7jzbpgzvvb";
  };
  propagatedBuildInputs = [ sh ];
  patches = ./blastdbget-no-validate-dbs.patch;
}
