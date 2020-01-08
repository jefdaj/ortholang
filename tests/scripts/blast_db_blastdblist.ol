dbs = blastdblist ""
nrs = blastdblist "nr"
proks = blastdblist "prok"
human = blastdblist "human"
result = dbs | nrs | proks | human
