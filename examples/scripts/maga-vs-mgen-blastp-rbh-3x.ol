# We got a list of homologs above, which is all we really want. But it's also
# important to make sure the list makes sense before wasting a lot of time
# trying to clone those genes! Since BLAST is nondeterministic, you can try
# running it a couple more times and only keep the consistent ("solid") hits.

# this is the same as pasting the contents of the other file here
include "maga-vs-mgen-blastp-rbh.ol"

# evaluate recHits 3 times and only keep the "solid" (consistent) homologs
n_repeats = 3
solidHits = repeat recHits mgen n_repeats
solidHitIDs = all (extract_queries_each solidHits)

result = solidHitIDs
