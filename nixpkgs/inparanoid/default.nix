with import ./..;

# TODO does this need to be mentioned in the main LICENSE file?

stdenv.mkDerivation rec {
  name = "inparanoid-${version}";
  version = "4.1";

  # Because of InParanoid's restrictive license, you have to enter your email here:
  # http://software.sbc.su.se/cgi-bin/request.cgi?project=inparanoid
  # They'll send you the tarball for personal/academic use. Put it in this folder.
  official_tarball = ./inparanoid_4.1.tar.gz;

  # This patch will edit the script to work with DIAMOND.
  # It's encrypted so it can be shared without violating the license.
  # (You can only read it once you have the official script)
  # TODO fetch from github
  encrypted_patch = ./patches-master.zip;

  buildInputs = [
    diamond
    ncbi-blast
    perlPackages.perl
    perlPackages.XMLParser
    openssl    # for the patch script i downloaded
    gnupg1orig # for the patch script i downloaded
    parallel
    unzip
  ];

  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source $stdenv/setup
    mkdir -p $TMPDIR
    cd $TMPDIR
    tar xvf ${official_tarball}
    unzip ${encrypted_patch}
    # yes, the tmpdir is hardcoded in the source
    mv patches-master/*.sh tmp/tmpmFSaYO/
    cd tmp/tmpmFSaYO
    sed -i 's/bin\/bash/usr\/bin\/env\ bash/g' *.sh
    export GNUPGHOME=$TMPDIR/gnupg
    mkdir -p $GNUPGHOME
    chmod 700 $GNUPGHOME
    ./apply-patch-inparanoid_4.1.sh
  '';
}
