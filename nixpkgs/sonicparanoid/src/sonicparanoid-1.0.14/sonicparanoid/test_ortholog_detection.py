'''Run main test functions for ortholog_detection module.'''
import sys, os
import ortholog_detection as orthodetect

debug = True

orthodetect.info()

#test inparanoid run
#orthodetect.test_run_inparanoid(debug=debug)

#test parallel inparanoid run
#orthodetect.test_run_inparanoid_parallel(local=local, debug=debug)

#test parallel inparanoid run using plast
#orthodetect.test_run_inparanoid_parallel_plast(local=local, debug=debug)

#test parallel inparanoid using only the first pass
#orthodetect.test_run_inparanoid_parallel_plast_1pass(debug=debug)

#test pyparanoid
#orthodetect.test_run_pyparanoid(local=local, debug=debug)

#execution time calculation
#orthodetect.test_calc_inparanoid_exec_time(debug=debug)

#calc stats about ortholog groups
#orthodetect.test_calc_ortholog_group_stats(debug=debug)

#blast_2pass
#orthodetect.test_blast_2pass(local=local, debug=debug)

#extract ortholog pairs
##orthodetect.test_extract_ortholog_pairs(debug=debug)

#fetch inparanoid tables
#orthodetect.test_fetch_inparanoid_tables(debug=debug)

#fetch sqltables
#orthodetect.test_fetch_sql_files(debug=debug)

# copy quickparanoid files
#orthodetect.test_copy_quickparanoid_files(debug=debug)

#run quickparanoid
#orthodetect.test_run_quickparanoid(debug=debug)

#extract only CORE orthologs
#orthodetect.test_filter_sql_tbl_core_orthologs(debug=debug)

#test blastp tab-separated InParanoid scores extraction
#orthodetect.test_inparanoid_like_parser(debug=debug)

# test alignment using mmseqs
#orthodetect.test_mmseqs_1pass(debug)

#test mmseqs2 alignment and parsing for two proteomes
#orthodetect.test_run_sonicparanoid2_parallel_mmseqs(debug=debug)

#test sonicparanoid using mmseqs2 on a proteome datasets
#orthodetect.test_run_sonicparanoid2(debug=False)

#test sonicparanoid using mmseqs2 on a proteome datasets
orthodetect.test_run_sonicparanoid2_multiproc(debug=False)
