{ stdenv, fetchurl, cmake, zlib }:

stdenv.mkDerivation rec {
  name = "diamond-0.9.22";

  src = fetchurl {
    url = "https://github.com/bbuchfink/diamond/archive/v0.9.22.tar.gz";
    sha256 = "0adp87r9ak63frdrdmrdfhsn6g0jnnyq1lr2wibvqbxcl37iir9m";
  };

  patches = [
    # ./diamond-0.9.22-no-warning.patch # TODO no longer needed?
  ];

  nativeBuildInputs = [ cmake ];
  buildInputs = [ zlib ];

  meta = with stdenv.lib; {
    description = "Accelerated BLAST compatible local sequence aligner";
    longDescription = ''
      A sequence aligner for protein and translated DNA
      searches and functions as a drop-in replacement for the NCBI BLAST
      software tools. It is suitable for protein-protein search as well as
      DNA-protein search on short reads and longer sequences including contigs
      and assemblies, providing a speedup of BLAST ranging up to x20,000.

      DIAMOND is developed by Benjamin Buchfink. Feel free to contact him for support (Email Twitter).

      If you use DIAMOND in published research, please cite
      B. Buchfink, Xie C., D. Huson,
      "Fast and sensitive protein alignment using DIAMOND",
      Nature Methods 12, 59-60 (2015).
        '';
    homepage = https://github.com/bbuchfink/diamond;
    license = {
      fullName = "University of Tuebingen, Benjamin Buchfink";
      url = https://raw.githubusercontent.com/bbuchfink/diamond/master/src/COPYING;
    };
    maintainers = [ maintainers.metabar ];
  };
}
