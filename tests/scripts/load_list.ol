small = load_list "examples/genome-lists/proteomes-small.txt"
refseq = load_list "examples/genome-lists/proteomes-refseq.txt"
result = small | refseq
