## BiomartR module

Search + download genomes and proteomes from Biomart.

Types:

| Extension | Meaning |
| :-------- | :------ |
| parse_searches |  |
| get_genomes |  |
| get_proteomes |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `parse_searches` | `str.list` | `search` |
| `get_genomes` | `str.list` | `fna.gz.list` |
| `get_proteomes` | `str.list` | `faa.gz.list` |


## BLAST+ module

Standard NCBI BLAST+ functions.

Types:

| Extension | Meaning |
| :-------- | :------ |
| blastn |  |
| megablast |  |
| blastp |  |
| blastx |  |
| tblastn |  |
| tblastx |  |
| blastn_each |  |
| megablast_each |  |
| blastp_each |  |
| blastx_each |  |
| tblastn_each |  |
| tblastx_each |  |
| blastn_db |  |
| megablast_db |  |
| blastp_db |  |
| blastx_db |  |
| tblastn_db |  |
| tblastx_db |  |
| blastn_db_each |  |
| megablast_db_each |  |
| blastp_db_each |  |
| blastx_db_each |  |
| tblastn_db_each |  |
| tblastx_db_each |  |
| concat_bht |  |
| concat_bht_each |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `blastn` | `num`, `fna`, `fna` | `bht` |
| `megablast` | `num`, `fna`, `fna` | `bht` |
| `blastp` | `num`, `faa`, `faa` | `bht` |
| `blastx` | `num`, `fna`, `faa` | `bht` |
| `tblastn` | `num`, `faa`, `fna` | `bht` |
| `tblastx` | `num`, `fna`, `fna` | `bht` |
| `blastn_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `megablast_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `blastp_each` | `num`, `faa`, `faa.list` | `bht.list` |
| `blastx_each` | `num`, `fna`, `faa.list` | `bht.list` |
| `tblastn_each` | `num`, `faa`, `fna.list` | `bht.list` |
| `tblastx_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `blastn_db` | `num`, `fna`, `ndb` | `bht` |
| `megablast_db` | `num`, `fna`, `ndb` | `bht` |
| `blastp_db` | `num`, `faa`, `pdb` | `bht` |
| `blastx_db` | `num`, `fna`, `pdb` | `bht` |
| `tblastn_db` | `num`, `faa`, `ndb` | `bht` |
| `tblastx_db` | `num`, `fna`, `ndb` | `bht` |
| `blastn_db_each` | `num`, `fna`, `ndb.list` | `bht.list` |
| `megablast_db_each` | `num`, `fna`, `ndb.list` | `bht.list` |
| `blastp_db_each` | `num`, `faa`, `pdb.list` | `bht.list` |
| `blastx_db_each` | `num`, `fna`, `pdb.list` | `bht.list` |
| `tblastn_db_each` | `num`, `faa`, `ndb.list` | `bht.list` |
| `tblastx_db_each` | `num`, `fna`, `ndb.list` | `bht.list` |
| `concat_bht` | `bht.list` | `bht` |
| `concat_bht_each` | `bht.list.list` | `bht.list` |


## CRB-BLAST module

Conditional reciprocal BLAST best hits (Aubry et al. 2014).

Types:

| Extension | Meaning |
| :-------- | :------ |
| crb_blast |  |
| crb_blast_each |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `crb_blast` | `fa`, `fa` | `crb` |
| `crb_blast_each` | `fa`, `fa.list` | `crb.list` |


## BlastDB module

Create, load, and download BLAST databases.

Types:

| Extension | Meaning |
| :-------- | :------ |
| load_nucl_db |  |
| load_prot_db |  |
| load_nucl_db_each |  |
| load_prot_db_each |  |
| makeblastdb_nucl_all |  |
| makeblastdb_prot_all |  |
| makeblastdb_nucl |  |
| makeblastdb_prot |  |
| makeblastdb_nucl_each |  |
| makeblastdb_prot_each |  |
| blastdbget |  |
| blastdblist |  |
| singletons |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `load_nucl_db` | `str` | `ndb` |
| `load_prot_db` | `str` | `pdb` |
| `load_nucl_db_each` | `str.list` | `ndb.list` |
| `load_prot_db_each` | `str.list` | `pdb.list` |
| `makeblastdb_nucl_all` | `fa.list` | `ndb` |
| `makeblastdb_prot_all` | `faa.list` | `pdb` |
| `makeblastdb_nucl:` | `fa` | `ndb` |
| `makeblastdb_prot` | `faa` | `pdb` |
| `makeblastdb_nucl_each` | `fa.list` | `ndb.list` |
| `makeblastdb_prot_each` | `faa.list` | `pdb.list` |
| `blastdbget` | `str` | `ndb` |
| `blastdblist` | `str` | `str.list` |
| `singletons` | `<whatever>.list` | `<whatever>.list.list` |


## BlastHits module

Work with BLAST hit tables.

Types:

| Extension | Meaning |
| :-------- | :------ |
| extract_queries |  |
| extract_queries_each |  |
| extract_targets |  |
| extract_targets_each |  |
| filter_evalue |  |
| filter_evalue_each |  |
| best_hits |  |
| best_hits_each |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `extract_queries` | `<crb/bht>` | `str.list` |
| `extract_queries_each` | `<crb/bht>.list` | `str.list.list` |
| `extract_targets` | `<crb/bht>` | `str.list` |
| `extract_targets_each` | `<crb/bht>.list` | `str.list.list` |
| `filter_evalue` | `num`, `bht` | `bht` |
| `filter_evalue_each` | `num`, `bht.list` | `bht.list` |
| `best_hits` | `bht` | `bht` |
| `best_hits_each` | `bht.list` | `bht.list` |


## BlastRBH module

Reciprocal BLAST+ best hits.

Types:

| Extension | Meaning |
| :-------- | :------ |
| blastn_rev |  |
| megablast_rev |  |
| blastp_rev |  |
| tblastx_rev |  |
| blastn_rev_each |  |
| megablast_rev_each |  |
| blastp_rev_each |  |
| tblastx_rev_each |  |
| reciprocal_best |  |
| reciprocal_best_each |  |
| blastn_rbh |  |
| megablast_rbh |  |
| blastp_rbh |  |
| tblastx_rbh |  |
| blastn_rbh_each |  |
| megablast_rbh_each |  |
| blastp_rbh_each |  |
| tblastx_rbh_each |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `blastn_rev` | `num`, `fna`, `fna` | `bht` |
| `megablast_rev` | `num`, `fna`, `fna` | `bht` |
| `blastp_rev` | `num`, `faa`, `faa` | `bht` |
| `tblastx_rev` | `num`, `fna`, `fna` | `bht` |
| `blastn_rev_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `megablast_rev_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `blastp_rev_each` | `num`, `faa`, `faa.list` | `bht.list` |
| `tblastx_rev_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `reciprocal_best` | `bht`, `bht` | `bht` |
| `reciprocal_best_each` | `bht`, `bht.list` | `bht.list` |
| `blastn_rbh` | `num`, `fna`, `fna` | `bht` |
| `megablast_rbh` | `num`, `fna`, `fna` | `bht` |
| `blastp_rbh` | `num`, `faa`, `faa` | `bht` |
| `tblastx_rbh` | `num`, `fna`, `fna` | `bht` |
| `blastn_rbh_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `megablast_rbh_each` | `num`, `fna`, `fna.list` | `bht.list` |
| `blastp_rbh_each` | `num`, `faa`, `faa.list` | `bht.list` |
| `tblastx_rbh_each` | `num`, `fna`, `fna.list` | `bht.list` |


## Load module

Load generic lists.

Types:

| Extension | Meaning |
| :-------- | :------ |
| load_list |  |
| glob_files |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `load_list` | `str` | `str.list` |
| `glob_files` | `str` | `str.list` |


## Length module

Get the lengths of lists and tables without printing them.

Types:

| Extension | Meaning |
| :-------- | :------ |
| length |  |
| length_each |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `length` | `<whatever>.list` | `num` |
| `length` | `<whatever>.list.list` | `num.list` |


## Math module

Basic math.

Types:

| Extension | Meaning |
| :-------- | :------ |
| + |  |
| - |  |
| * |  |
| / |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `+` | `num`, `num` | `num` |
| `-` | `num`, `num` | `num` |
| `*` | `num`, `num` | `num` |
| `/` | `num`, `num` | `num` |


## MUSCLE module

Align sequences with MUSCLE.

Types:

| Extension | Meaning |
| :-------- | :------ |
| muscle |  |
| muscle_each |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `muscle` | `faa` | `aln` |
| `muscle_each` | `faa.list` | `aln.list` |


## Permute module

Generate random permutations of lists.

Types:

| Extension | Meaning |
| :-------- | :------ |
| leave_each_out |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `leave_each_out` | `<whatever>.list` | `<whatever>.list.list` |


## Repeat module

Repeatdly re-calculate variables using different random seeds.

Types:

| Extension | Meaning |
| :-------- | :------ |
| repeat_each |  |
| repeat |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `repeat_each` | `<outputvar>`, `<inputvar>`, `<inputvars>` | `<output>.list` |
| `repeat` | `<outputvar>`, `<inputvar>`, `num` | `<output>.list` |


## SeqIO module

Sequence file manipulations using BioPython's SeqIO.

Types:

| Extension | Meaning |
| :-------- | :------ |
| gbk_to_faa |  |
| gbk_to_faa_each |  |
| gbk_to_fna |  |
| gbk_to_fna_each |  |
| extract_seqs |  |
| extract_seqs_each |  |
| extract_ids |  |
| extract_ids_each |  |
| translate |  |
| translate_each |  |
| concat_fna |  |
| concat_fna_each |  |
| concat_faa |  |
| concat_faa_each |  |
| split_faa |  |
| split_faa_each |  |
| split_fna |  |
| split_fna_each |  |
| load_fna |  |
| load_fna_each |  |
| load_fna_glob |  |
| load_faa |  |
| load_faa_each |  |
| load_faa_glob |  |
| load_gbk |  |
| load_gbk_each |  |
| load_gbk_glob |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `gbk_to_faa` | `gbk` | `faa` |
| `gbk_to_faa_each` | `gbk.list` | `faa.list` |
| `gbk_to_fna` | `gbk` | `fna` |
| `gbk_to_fna_each` | `gbk.list` | `fna.list` |
| `extract_seqs` | `fa` | `str.list` |
| `extract_seqs_each` | `fa.list` | `str.list.list` |
| `extract_ids` | `fa` | `str.list` |
| `extract_ids_each` | `fa.list` | `str.list.list` |
| `translate` | `fna` | `faa` |
| `translate_each` | `fna.list` | `faa.list` |
| `concat_fna` | `fna.list` | `fna` |
| `concat_fna_each` | `fna.list.list` | `fna.list` |
| `concat_faa` | `faa.list` | `faa` |
| `concat_faa_each` | `faa.list.list` | `faa.list` |
| `split_faa` | `faa` | `faa.list` |
| `split_faa_each` | `faa.list` | `faa.list.list` |
| `split_fna` | `fna` | `fna.list` |
| `split_fna_each` | `fna.list` | `fna.list.list` |
| `load_fna` | `str` | `fna` |
| `load_fna_each` | `str.list` | `fna.list` |
| `load_fna_glob` | `str` | `fna.list` |
| `load_faa` | `str` | `faa` |
| `load_faa_each` | `str.list` | `faa.list` |
| `load_faa_glob` | `str` | `faa.list` |
| `load_gbk` | `str` | `gbk` |
| `load_gbk_each` | `str.list` | `gbk.list` |
| `load_gbk_glob` | `str` | `gbk.list` |


## Sets module

Set operations for use with lists.

Types:

| Extension | Meaning |
| :-------- | :------ |
| some |  |
| | |  |
| any |  |
| & |  |
| all |  |
| ~ |  |
| diff |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `some` | `<whatever>.list.list` | `<whatever>.list` |
| `|` | `<whatever>.list`, `<whatever>.list` | `<whatever>.list` |
| `any` | `<whatever>.list.list` | `<whatever>.list` |
| `&` | `<whatever>.list`, `<whatever>.list` | `<whatever>.list` |
| `all` | `<whatever>.list.list` | `<whatever>.list` |
| `~` | `<whatever>.list`, `<whatever>.list` | `<whatever>.list` |
| `diff` | `<whatever>.list.list` | `<whatever>.list` |


## Sample module

Random (but reproducable) sampling of list elements.

Types:

| Extension | Meaning |
| :-------- | :------ |
| sample |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `sample` | `<whatever>.list` | `<whatever>.list` |


## Summarize module

Collapse a list of results into a single summary.

Types:

| Extension | Meaning |
| :-------- | :------ |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |


## Scores module

Score repeated variables for plotting.

Types:

| Extension | Meaning |
| :-------- | :------ |
| score_repeats |  |
| extract_scores |  |
| extract_scored |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `score_repeats` | `<outputnum>`, `<inputvar>`, `<inputlist>` | `<input>.scores` |
| `extract_scores` | `<whatever>.scores` | `num.list` |
| `extract_scored` | `<whatever>.scores` | `<whatever>.list` |


## Plots module

Generate half-decent plots.

Types:

| Extension | Meaning |
| :-------- | :------ |
| histogram |  |
| linegraph |  |
| scatterplot |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `histogram` | `str`, `num.list` | `plot` |
| `linegraph` | `str`, `num.scores` | `plot` |
| `scatterplot` | `str`, `num.scores` | `plot` |


## PsiBLAST module

PsiBLAST (BLAST+) searches using position-specific sequence matrixes.

Types:

| Extension | Meaning |
| :-------- | :------ |
| psiblast |  |
| psiblast_all |  |
| psiblast_db |  |
| psiblast_db_each |  |
| psiblast_each |  |
| psiblast_each_pssm |  |
| psiblast_each_pssm_db |  |
| psiblast_pssm |  |
| psiblast_pssm_all |  |
| psiblast_pssm_db |  |
| psiblast_pssm_db_each |  |
| psiblast_pssm_each |  |
| psiblast_pssms |  |
| psiblast_pssms_all |  |
| psiblast_pssms_db |  |
| psiblast_train |  |
| psiblast_train_all |  |
| psiblast_train_db |  |
| psiblast_train_db_each |  |
| psiblast_train_each |  |
| psiblast_train_pssms |  |
| psiblast_train_pssms_db |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `psiblast` | `num`, `faa`, `faa` | `bht` |
| `psiblast_all` | `num`, `faa`, `faa.list` | `bht` |
| `psiblast_db` | `num`, `faa`, `pdb` | `bht` |
| `psiblast_db_each` | `num`, `faa`, `pdb.list` | `bht.list` |
| `psiblast_each` | `num`, `faa`, `faa.list` | `bht.list` |
| `psiblast_each_pssm` | `num`, `pssm.list`, `faa` | `bht.list` |
| `psiblast_each_pssm_db` | `num`, `pssm.list`, `pdb` | `bht.list` |
| `psiblast_pssm` | `num`, `pssm`, `faa` | `bht` |
| `psiblast_pssm_all` | `num`, `pssm`, `faa.list` | `bht` |
| `psiblast_pssm_db` | `num`, `pssm`, `pdb` | `bht` |
| `psiblast_pssm_db_each` | `num`, `pssm`, `pdb.list` | `bht.list` |
| `psiblast_pssm_each` | `num`, `pssm`, `faa.list` | `bht.list` |
| `psiblast_pssms` | `num`, `pssm.list`, `faa` | `bht` |
| `psiblast_pssms_all` | `num`, `pssm.list`, `faa` | `bht` |
| `psiblast_pssms_db` | `num`, `pssm.list`, `pdb` | `bht` |
| `psiblast_train` | `num`, `faa`, `faa` | `pssm` |
| `psiblast_train_all` | `num`, `faa`, `faa.list` | `pssm` |
| `psiblast_train_db` | `num`, `faa`, `pdb` | `pssm` |
| `psiblast_train_db_each` | `num`, `faa`, `pdb.list` | `pssm.list` |
| `psiblast_train_each` | `num`, `faa`, `faa.list` | `pssm.list` |
| `psiblast_train_pssms` | `num`, `faa.list`, `faa` | `pssm.list` |
| `psiblast_train_pssms_db` | `num`, `faa.list`, `pdb` | `pssm.list` |


## HMMER module

Search sequences with hidden Markov models.

Types:

| Extension | Meaning |
| :-------- | :------ |
| hmmbuild |  |
| hmmbuild_each |  |
| hmmsearch |  |
| hmmsearch_each |  |
| extract_hmm_targets |  |
| extract_hmm_targets_each |  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `hmmbuild` | `aln` | `hmm` |
| `hmmbuild_each` | `aln.list` | `hmm.list` |
| `hmmsearch` | `num`, `hmm`, `faa` | `hht` |
| `hmmsearch_each` | `num`, `hmm.list`, `faa` | `hht.list` |
| `extract_hmm_targets` | `hht` | `str.list` |
| `extract_hmm_targets_each` | `hht.list` | `str.list.list` |


