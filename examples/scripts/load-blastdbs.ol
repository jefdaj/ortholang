microbial16s = blastdbget_fna "16SMicrobial"
env_nt = blastdbget_fna "env_nt"
est = blastdbget_fna "est"
est_human = blastdbget_fna "est_human"
est_human_blob = blastdbget_fna "est_human_blob"
est_mouse = blastdbget_fna "est_mouse"
est_mouse_blob = blastdbget_fna "est_mouse_blob"
est_others = blastdbget_fna "est_others"
gss = blastdbget_fna "gss"
htgs = blastdbget_fna "htgs"
human_genomic = blastdbget_fna "human_genomic"
nt = blastdbget_fna "nt"
other_genomic = blastdbget_fna "other_genomic"
patnt = blastdbget_fna "patnt"
pdbnt = blastdbget_fna "pdbnt"
ref_prok_rep_genomes = blastdbget_fna "ref_prok_rep_genomes"
ref_viroids_rep_genomes = blastdbget_fna "ref_viroids_rep_genomes"
ref_viruses_rep_genomes = blastdbget_fna "ref_viruses_rep_genomes"
refseq_genomic = blastdbget_fna "refseq_genomic"
refseq_rna = blastdbget_fna "refseq_rna"
refseqgene = blastdbget_fna "refseqgene"
sts = blastdbget_fna "sts"
tsa_nt = blastdbget_fna "tsa_nt"
vector = blastdbget_fna "vector"

ndbs = [microbial16s, env_nt, est, est_human, est_human_blob, est_mouse,
        est_mouse_blob, est_others, gss, htgs, human_genomic, nt,
        other_genomic, patnt, pdbnt, ref_prok_rep_genomes,
        ref_viroids_rep_genomes, ref_viruses_rep_genomes, refseq_genomic,
        refseq_rna, refseqgene, sts, tsa_nt, vector]

cdd_delta = blastdbget_faa "cdd_delta"
env_nr = blastdbget_faa "env_nr"
landmark = blastdbget_faa "landmark"
nr = blastdbget_faa "nr"
pataa = blastdbget_faa "pataa"
pdbaa = blastdbget_faa "pdbaa"
refseq_protein = blastdbget_faa "refseq_protein"
swissprot = blastdbget_faa "swissprot"
tsa_nr = blastdbget_faa "tsa_nr"

pdbs = [cdd_delta, env_nr, landmark, nr, pataa, pdbaa, refseq_protein,
        swissprot, tsa_nr]

# TODO make it valid to use different types from a type group in a list
#      (but only for typegroup-specified functions)
# result = length_each [ndbs, pdbs]
result = "nothing so far"
