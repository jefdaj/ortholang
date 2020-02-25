# Finally, let's get a list of the ones that were found all 100 times. We
# don't need any of the plotting code for that, so we can go back to the second
# script...

include "maga-vs-mgen-blastp-rbh-3x.ol"
solidHitIDs100 = all (repeat solidHitIDs n_repeats 100)
result = solidHitIDs100
