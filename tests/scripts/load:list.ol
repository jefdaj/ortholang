# TODO is this how we should handle the paths thing?
small  = to_path_each (load_list "examples/genome-lists/proteomes-small.txt")
refseq = to_path_each (load_list "examples/genome-lists/proteomes-refseq.txt")
result = small | refseq
