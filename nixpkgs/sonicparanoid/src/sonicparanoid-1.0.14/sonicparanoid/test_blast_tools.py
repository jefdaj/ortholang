'''Run main test functions for blast_tools module.'''
import sys, os
import blast_tools as btools

debug = True

btools.info()



#test the update of the paths
#btools.test_paths_update(debug)

#test blastp run
#btools.test_blastn(debug)

#test blastp run
#btools.test_blastp(debug)

#test blastx run
btools.test_blastx(debug)

#test lineage extraction
#btools.test_getNcbiLineageKc(debug)

#test lineage extraction
#btools.test_getNcbiLineageKcVirus(debug)

#test lineage extraction
#btools.test_extractBlastnBestHitsVirusFromTable(debug)

#test best hits extraction
#btools.test_extractBlastnBestHits(debug)

#test best hits extraction
#btools.test_extractBlastnBestHitsKc(debug)

#test best hits extraction for virus only
#btools.test_extractBlastnBestHitsKcVirus(debug)

#test blast output conversion
#btools.test_convertBlastOutput(debug)
