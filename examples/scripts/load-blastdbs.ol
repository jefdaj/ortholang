microbial16s = blastdbget_nucl "16SMicrobial" 
env_nt = blastdbget_nucl "env_nt" 
est = blastdbget_nucl "est" 
est_human = blastdbget_nucl "est_human" 
est_human_blob = blastdbget_nucl "est_human_blob" 
est_mouse = blastdbget_nucl "est_mouse" 
est_mouse_blob = blastdbget_nucl "est_mouse_blob" 
est_others = blastdbget_nucl "est_others" 
gss = blastdbget_nucl "gss" 
htgs = blastdbget_nucl "htgs" 
human_genomic = blastdbget_nucl "human_genomic" 
nt = blastdbget_nucl "nt" 
other_genomic = blastdbget_nucl "other_genomic" 
patnt = blastdbget_nucl "patnt" 
pdbnt = blastdbget_nucl "pdbnt" 
ref_prok_rep_genomes = blastdbget_nucl "ref_prok_rep_genomes" 
ref_viroids_rep_genomes = blastdbget_nucl "ref_viroids_rep_genomes" 
ref_viruses_rep_genomes = blastdbget_nucl "ref_viruses_rep_genomes" 
refseq_genomic = blastdbget_nucl "refseq_genomic" 
refseq_rna = blastdbget_nucl "refseq_rna" 
refseqgene = blastdbget_nucl "refseqgene" 
sts = blastdbget_nucl "sts" 
tsa_nt = blastdbget_nucl "tsa_nt" 
vector = blastdbget_nucl "vector" 

ndbs = [microbial16s, env_nt, est, est_human, est_human_blob, est_mouse,
        est_mouse_blob, est_others, gss, htgs, human_genomic, nt,
        other_genomic, patnt, pdbnt, ref_prok_rep_genomes,
        ref_viroids_rep_genomes, ref_viruses_rep_genomes, refseq_genomic,
        refseq_rna, refseqgene, sts, tsa_nt, vector]

cdd_delta = blastdbget_prot "cdd_delta" 
env_nr = blastdbget_prot "env_nr" 
landmark = blastdbget_prot "landmark" 
nr = blastdbget_prot "nr" 
pataa = blastdbget_prot "pataa" 
pdbaa = blastdbget_prot "pdbaa" 
refseq_protein = blastdbget_prot "refseq_protein" 
swissprot = blastdbget_prot "swissprot" 
tsa_nr = blastdbget_prot "tsa_nr" 

pdbs = [cdd_delta, env_nr, landmark, nr, pataa, pdbaa, refseq_protein,
        swissprot, tsa_nr]

# TODO make it valid to use different types from a type group in a list
#      (but only for typegroup-specified functions)
# result = length_each [ndbs, pdbs]
result = "nothing so far"
