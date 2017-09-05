{ pythonPackages, fetchgit }:
with pythonPackages;

buildPythonPackage {
  name = "blastdbget";
  version = "2015-07"; # TODO is that right?
  prefix = "";
  src = fetchgit {
    url = "https://github.com/brwnj/blastdbget";
    rev = "98ba2871cb3ceb29b722926351374e1eae45cb75";
    sha256 = "0kfhqf03b52v5bbyi8rjx4lp1ncgvmy5fbzdbvgjnf7jzbpgzvvb";
  };
  propagatedBuildInputs = [ sh ];
}
