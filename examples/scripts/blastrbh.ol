# query = load_
# subject = load_
e = 1e-20

# for simple cases, one _rbh call is enough:
# rbh_hits = 

# but you can also separate the forward and reverse searches,
# useful when you want to use different settings or inspect the hits.
# these give the same result as rbh_hits above:
# fwd_hits = 
# rev_hits = 
# rbh_hits_2 = reciprocal_best fwd_hits rev_hits

# the _rev functions are like their forward counterparts with the input files flipped.
# they just reduce the chances of messing up the order.
# for example, this is the same as rev_hits above:
# rev_hits_2 = 

result = "example not written yet"
