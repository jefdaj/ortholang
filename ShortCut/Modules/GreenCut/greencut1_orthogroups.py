
# simplest algorithm:
#
# take pident cutoff, list of rb_hits tables, list of more_hits tables
#
# for each zipped (rb_hits, more_hits) pair:
#   make a set of rb_hits qids
#   seed orthogoups from those qids
#   add more_hits sids above the pident cutoff to their orthogroup
#
# ...

# Simpler algorithm without loading the chlamy fasta:
# take cutoff fraction, list of rb_hits tables, list of more_hits tables
# for each zipped (rb_hits, more_hits) pair:
#   make a dict of qid:(cutoff, orthogroup) mappings
#     calculate pdist cutoff
#     assume the first one (qid?) is from the reference species
#     seed the orthogroup with the qid
#   for each hit in the more_hits table:
#     look up the qid in the orthogroups dict
#     ...


