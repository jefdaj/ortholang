{ mcl, diamond, hmmer, pythonPackages }: # TODO muscle
with pythonPackages;

# from inparanoid:
# stdenv.mkDerivation rec {
#   name = "sonicparanoid-${version}";
#   version = "0.2.2";
# 
#   src = 
# 
#   buildInputs = [
#     diamond
#     psiblast-exb
#     perlPackages.perl
#     perlPackages.XMLParser
#     openssl    # for the patch script i downloaded
#     gnupg1orig # for the patch script i downloaded
#     parallel
#     unzip
#   ];
# 
#   builder = writeScript "builder.sh" ''
#     #!/usr/bin/env bash
#     source $stdenv/setup
#     mkdir -p $TMPDIR
#     cd $TMPDIR
#     tar xvf ${official_tarball}
#     unzip ${encrypted_patch}
#     # yes, the tmpdir is hardcoded in the source
#     mv patches-master/*.sh tmp/tmpmFSaYO/
#     cd tmp/tmpmFSaYO
#     sed -i 's/bin\/bash/usr\/bin\/env\ bash/g' *.sh
#     export GNUPGHOME=$TMPDIR/gnupg
#     mkdir -p $GNUPGHOME
#     chmod 700 $GNUPGHOME
#     ./apply-patch-inparanoid_4.1.sh
#   '';
# }

# from biopython:
# { lib
# , buildPythonPackage
# , fetchPypi
# , numpy
# }:

buildPythonPackage rec {
  pname = "sonicparanoid";
  version = "0.2.2";

  src = ./sonicparanoid;
  # src = fetchPypi {
  #   inherit pname version;
  #   sha256 = "4a7c5298f03d1a45523f32bae1fffcff323ea9dce007fb1241af092f5ab2e45b";
  # };

  # TODO just regular buildInputs? or wrap the exe?
  propagatedBuildInputs = [
    diamond
    hmmer
    mcl
    # TODO muscle 
    # TODO package cd-hit? http://weizhongli-lab.org/cd-hit/
  ];

  # Checks try to write to $HOME, which does not work with nix
  # doCheck = false;

  meta = {
    # description = "Python library for bioinformatics";
    # longDescription = ''
    #   Biopython is a set of freely available tools for biological computation
    #   written in Python by an international team of developers. It is a
    #   distributed collaborative effort to develop Python libraries and
    #   applications which address the needs of current and future work in
    #   bioinformatics.
    # '';
    # homepage = http://biopython.org/wiki/Documentation;
    # maintainers = with lib.maintainers; [ luispedro ];
    # license = lib.licenses.bsd3;
  };
}
