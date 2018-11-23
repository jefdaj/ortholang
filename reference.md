## Math module

Basic math.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `+` | `num`, `num` | `num` |
| `-` | `num`, `num` | `num` |
| `*` | `num`, `num` | `num` |
| `/` | `num`, `num` | `num` |


## Load module

Load generic lists.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `load_list` | `str` | `str.list` |
| `glob_files` | `str` | `str.list` |


## Sets module

Set operations for use with lists.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `some` | `X.list.list` | `X.list` |
| `|` | `X.list`, `X.list` | `X.list` |
| `any` | `X.list.list` | `X.list` |
| `&` | `X.list`, `X.list` | `X.list` |
| `all` | `X.list.list` | `X.list` |
| `~` | `X.list`, `X.list` | `X.list` |
| `diff` | `X.list.list` | `X.list` |


## SeqIO module

Sequence file manipulations using BioPython's SeqIO.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `gbk` | genbank file |
| `faa` | FASTA (amino acid) |
| `fna` | FASTA (nucleic acid) |

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


## BiomartR module

Search + download genomes and proteomes from Biomart.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `search` | intermediate table describing biomartr searches |
| `fna.gz` | gzipped fasta nucleic acid acid (gene list or genome) |
| `faa.gz` | gzipped fasta amino acid (proteome) |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `parse_searches` | `str.list` | `search` |
| `get_genomes` | `str.list` | `fna.gz.list` |
| `get_proteomes` | `str.list` | `faa.gz.list` |


## BlastDB module

Create, load, and download BLAST databases.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `ndb` | BLAST nucleotide database |
| `pdb` | BLAST protein database |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `load_nucl_db` | `str` | `ndb` |
| `load_prot_db` | `str` | `pdb` |
| `load_nucl_db_each` | `str.list` | `ndb.list` |
| `load_prot_db_each` | `str.list` | `pdb.list` |
| `makeblastdb_nucl_all` | `fa.list` | `ndb` |
| `makeblastdb_prot_all` | `faa.list` | `pdb` |
| `makeblastdb_nucl` | `fa` | `ndb` |
| `makeblastdb_prot` | `faa` | `pdb` |
| `makeblastdb_nucl_each` | `fa.list` | `ndb.list` |
| `makeblastdb_prot_each` | `faa.list` | `pdb.list` |
| `blastdbget` | `str` | `ndb` |
| `blastdblist` | `str` | `str.list` |
| `singletons` | `X.list` | `X.list.list` |


## BLAST+ module

Standard NCBI BLAST+ functions.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `ndb` | BLAST nucleotide database |
| `pdb` | BLAST protein database |
| `bht` | tab-separated table of blast hits (outfmt 6) |

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


## BlastHits module

Work with BLAST hit tables.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `bht` | tab-separated table of blast hits (outfmt 6) |
| `crb` | tab-separated table of conditional reciprocal blast best hits |

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


## Length module

Get the lengths of lists and tables without printing them.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `bht` | tab-separated table of blast hits (outfmt 6) |
| `crb` | tab-separated table of conditional reciprocal blast best hits |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `length` | `X.list` | `num` |
| `length` | `X.list.list` | `num.list` |


## PsiBLAST module

Iterated PsiBLAST (BLAST+) searches using position-specific substitution matrixes.

There are a lot of these! Some naming conventions:

* A fn with `train` trains and returns one or more pssms ; one without
`train` runs a regular blast search and returns hits.

* A fn with `db` takes one or more blast databases directly; one without
`db` auto-builds the db(s) from one or more fastas.

* A fn with `all` takes a list of fastas and creates one db from it.

* A fn with `each` maps over its last argument. The difference between
`each` and `all` is that `each` returns a list of results, whereas `all`
summarizes them into one thing.

* A fn with `pssms` (plural) takes a list of pssm queries and combines
their hits into one big table.

So for example...


```
psiblast_train_all : num faa faa.list -> pssm
  auto-builds one blast db from a list of fasta files
  trains a pssm for the query fasta on it
  returns the pssm
```

```
psiblast_each : num faa faa.list -> bht.list
  auto-builds one db per subject fasta
  trains a pssm for the query fasta against each one
  runs a final psiblast search against each one using the pssm
  returns a list of hit tables
```

TODO individual help descriptions for each fn.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `pdb` | BLAST protein database |
| `bht` | tab-separated table of blast hits (outfmt 6) |
| `pssm` | PSI-BLAST position-specific substitution matrix as ASCII |

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


## CRB-BLAST module

Conditional reciprocal BLAST best hits (Aubry et al. 2014).

Types:

| Extension | Meaning |
| :-------- | :------ |
| `fna` | FASTA (nucleic acid) |
| `faa` | FASTA (amino acid) |
| `crb` | tab-separated table of conditional reciprocal blast best hits |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `crb_blast` | `fa`, `fa` | `crb` |
| `crb_blast_each` | `fa`, `fa.list` | `crb.list` |


## HMMER module

Search sequences with hidden Markov models.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `aln` | multiple sequence alignment |
| `hmm` | hidden markov model |
| `hht` | HMMER hits table |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `hmmbuild` | `aln` | `hmm` |
| `hmmbuild_each` | `aln.list` | `hmm.list` |
| `hmmsearch` | `num`, `hmm`, `faa` | `hht` |
| `hmmsearch_each` | `num`, `hmm.list`, `faa` | `hht.list` |
| `extract_hmm_targets` | `hht` | `str.list` |
| `extract_hmm_targets_each` | `hht.list` | `str.list.list` |


## BlastRBH module

Reciprocal BLAST+ best hits.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `ndb` | BLAST nucleotide database |
| `pdb` | BLAST protein database |
| `bht` | tab-separated table of blast hits (outfmt 6) |

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


## MUSCLE module

Align sequences with MUSCLE.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `aln` | multiple sequence alignment |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `muscle` | `faa` | `aln` |
| `muscle_each` | `faa.list` | `aln.list` |


## Sample module

Random (but reproducable) sampling of list elements.

WARNING: Because of the way ShortCut caches tempfiles, calling these
more than once will give the same sublist each time! For different
sublists, use in combination with the 'repeat' function.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `sample` | `num`, `X.list` | `X.list` |


## Permute module

Generate random permutations of lists.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `leave_each_out` | `X.list` | `X.list.list` |


## Repeat module

Repeatdly re-calculate variables using different random seeds.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `repeat_each` | `<outputvar>`, `<inputvar>`, `<inputvars>` | `<output>.list` |
| `repeat` | `<outputvar>`, `<inputvar>`, `num` | `<output>.list` |


## Summarize module

Collapse a list of results into a single summary.




## Scores module

Score repeated variables for plotting.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `score_repeats` | `<outputnum>`, `<inputvar>`, `<inputlist>` | `<input>.scores` |
| `extract_scores` | `X.scores` | `num.list` |
| `extract_scored` | `X.scores` | `X.list` |


## Plots module

Generate half-decent plots.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `png` | png image of a plot |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `histogram` | `str`, `num.list` | `plot` |
| `linegraph` | `str`, `num.scores` | `plot` |
| `scatterplot` | `str`, `num.scores` | `plot` |


## OrthoFinder module

Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree.

Types:

| Extension | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `ofr` | OrthoFinder results |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `orthofinder` | `faa.list` | `ofr` |
| `orthogroups` | `ofr` | `str.list.list` |
| `orthogroup_containing` | `ofr`, `str` | `str.list` |
| `orthogroups_containing` | `ofr`, `str.list` | `str.list.list` |


