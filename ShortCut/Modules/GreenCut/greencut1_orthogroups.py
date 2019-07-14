# This isn't written at all yet...

#
# one algorithm:
#
# take pident cutoff, list of rb_hits tables, list of more_hits tables
#
# for each zipped (rb_hits, more_hits) pair:
#   make a set of rb_hits qids
#   seed orthogoups from those qids
#   add more_hits sids above the pident cutoff to their orthogroup
#
# ...

# another variation:
# take cutoff fraction, list of rb_hits tables, list of more_hits tables
# for each zipped (rb_hits, more_hits) pair:
#   make a dict of qid:(cutoff, orthogroup) mappings
#     calculate pdist cutoff
#     assume the first one (qid?) is from the reference species
#     seed the orthogroup with the qid
#   for each hit in the more_hits table:
#     look up the qid in the orthogroups dict
#     ...


# TODO is this only needed for the greencut1 algorithm, not 2?
def calc_pdists(gen):
    pdists = {}
    for hit in gen:
        if not hit.qid in pdists:
            pdists[hit.qid] = {}
        # TODO in rb_hits, there should only ever be one sid right?
        if not hit.sid in pdists[hit.qid]:
            pdists[hit.qid][hit.sid] = []
        assert hit.pident >= 0.0, hit.pident <= 100.0 # TODO rounding errors here?
        # TODO calculated like they say in the methods, will this exclude all paralogs in chlamy?
        pdist = 1 - (hit.pident / 100)
        # print(hit.__dict__)
        pdists[hit.qid][hit.sid].append(pdist)
    return pdists
