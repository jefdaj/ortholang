l1 = range_integers 1 10
l2 = range_integers 3 15
l3 = range_integers 3 20
l4 = range_integers 15 50
l5 = range_integers 0 10
l6 = range_integers 0 100

d_one   = sets_table [l1]
d_two   = sets_table [l1, l2]
d_three = sets_table [l1, l2, l3]
d_four  = sets_table [l1, l2, l3, l4]
d_five  = sets_table [l1, l2, l3, l4, l5]
d_six   = sets_table [l1, l2, l3, l4, l5, l6]

# this caught a bug in the label ordering once
d_five_unordered = sets_table [l2,l5,l1,l3,l4]

result = [d_one, d_two, d_three, d_four, d_five, d_five_unordered, d_six]
