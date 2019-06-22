#run the main test functions in the workers.py module
import process_output as po

debug = True

po.info()

# test output prerpocessing
#po.test_process_multisp_tbl(debug=debug)

# test extraction by species in clstrs
#po.test_extract_by_sp_cnt(debug=debug)

# test extraction by cluster IDs
#po.test_extract_by_id(debug=debug)

# test FASTA sequence extration
#po.test_extract_fasta(debug=debug)

# test fasta extraction
#po.test_load_seqs_in_dict(debug=debug)

# test loading of the annotation files
po.test_load_annotations(debug=debug)
