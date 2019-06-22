'''Run main test functions for seq_tools module.'''
import sys, os
import seq_tools as seqtools

debug = True

seqtools.info()


#test reads correction
#seqtools.test_correctIllumina(debug)

#test count sequences
#seqtools.test_countSeqs(debug)

#test file format check
#seqtools.test_checkSeqFormat(debug)

#test file fasta type DNA OR PROTEIN
#seqtools.test_checkMoleculeType(debug)

#test dna extraction from genbank
#seqtools.test_gbk2fna(debug)

#test proteins extraction from genbank
#seqtools.test_gbk2faa(debug)

#test if the input gbk contains a phage
#seqtools.test_isPhage(debug)

#check the extraction of the project id
#seqtools.test_getBioprjFromGbk(debug)

#make histogram of seq lengths
#seqtools.test_makeLenghtHist(debug)

# search for blanks in hdrs
#seqtools.test_checkFastaHdrForBlanks(debug)

# reformat headers
seqtools.test_formatFastaHdr(debug)
