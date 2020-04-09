# list available BLAST databases with "human" in their names
human_dbs = blastdblist "human"

# download the SwissProt database
swissprot = blastdbget_faa "swissprot"

result = human_dbs
