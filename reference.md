{% import "macros.jinja" as macros with context %}

<input id="modulesearch" placeholder="Search the module documentation" id="box" type="text"/>

If you don't find what you're looking for, leave Jeff a comment about it! (bottom right)
<br/>

<div class="moduleblock">
<h3>Replace module</h3>

Replace variables in the script to see how the results change.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `replace` | `<outputvar>`, `<vartoreplace>`, `<exprtoreplacewith>` | `<newoutput>` |
| `replace_each` | `<outputvar>`, `<inputvar>`, `<inputvars>` | `<output>.list` |

<br/>
{{ macros.load_cut(user, 'examples/replace.cut') }}
</div>
<div class="moduleblock">
<h3>Repeat module</h3>

Repeatdly re-calculate variables using different random salts.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `repeat` | `<outputvar>`, `<inputvar>`, `num` | `<output>.list` |

<br/>
{{ macros.load_cut(user, 'examples/repeat.cut') }}
</div>
<div class="moduleblock">
<h3>Math module</h3>

Basic math.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `+` | `num`, `num` | `num` |
| `-` | `num`, `num` | `num` |
| `*` | `num`, `num` | `num` |
| `/` | `num`, `num` | `num` |

<br/>
{{ macros.load_cut(user, 'examples/math.cut') }}
</div>
<div class="moduleblock">
<h3>Load module</h3>

Load generic lists.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `load_list` | `str` | `str.list` |
| `glob_files` | `str` | `str.list` |

<br/>
{{ macros.load_cut(user, 'examples/load.cut') }}
</div>
<div class="moduleblock">
<h3>Sets module</h3>

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

<br/>
{{ macros.load_cut(user, 'examples/sets.cut') }}
</div>
<div class="moduleblock">
<h3>SeqIO module</h3>

Sequence file manipulations using BioPython's SeqIO.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `gbk` | genbank |
| `faa` | FASTA (amino acid) |
| `fna` | FASTA (nucleic acid) |
| `fa` | FASTA (nucleic OR amino acid) |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `gbk_to_faa` | `gbk` | `faa` |
| `gbk_to_faa_each` | `gbk.list` | `faa.list` |
| `gbk_to_fna` | `gbk` | `fna` |
| `gbk_to_fna_each` | `gbk.list` | `fna.list` |
| `extract_seqs` | `fa`, `str.list` | `fa` |
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

<br/>
{{ macros.load_cut(user, 'examples/seqio.cut') }}
</div>
<div class="moduleblock">
<h3>BiomartR module</h3>

Search + download genomes and proteomes from Biomart.

Types:

| Type      | Meaning |
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

<br/>
{{ macros.load_cut(user, 'examples/biomartr.cut') }}
</div>
<div class="moduleblock">
<h3>BlastDB module</h3>

Create, load, and download BLAST databases.

Types:

| Type      | Meaning |
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
| `blastdbget_nucl` | `str` | `ndb` |
| `blastdbget_prot` | `str` | `pdb` |
| `blastdblist` | `str` | `str.list` |
| `singletons` | `X.list` | `X.list.list` |

<br/>
{{ macros.load_cut(user, 'examples/blastdb.cut') }}
</div>
<div class="moduleblock">
<h3>BLAST+ module</h3>

Standard NCBI BLAST+ functions.

Types:

| Type      | Meaning |
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

<br/>
{{ macros.load_cut(user, 'examples/blast.cut') }}
</div>
<div class="moduleblock">
<h3>BlastHits module</h3>

Work with BLAST hit tables.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `bht` | tab-separated table of blast hits (outfmt 6) |
| `crb` | tab-separated table of conditional reciprocal blast best hits |
| `hittable` | files in  |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `extract_queries` | `hittable` | `str.list` |
| `extract_queries_each` | `hittable.list` | `str.list.list` |
| `extract_targets` | `hittable` | `str.list` |
| `extract_targets_each` | `hittable.list` | `str.list.list` |
| `filter_evalue` | `num`, `hittable` | `bht` |
| `filter_evalue_each` | `num`, `hittable.list` | `bht.list` |
| `best_hits` | `hittable` | `bht` |
| `best_hits_each` | `hittable.list` | `bht.list` |

<br/>
{{ macros.load_cut(user, 'examples/blasthits.cut') }}
</div>
<div class="moduleblock">
<h3>ListLike module</h3>

Operations on files that can be treated like lists.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `bht` | tab-separated table of blast hits (outfmt 6) |
| `crb` | tab-separated table of conditional reciprocal blast best hits |
| `mms` | MMSeqs2 sequence database |
| `listlike` | files that can be treated like lists |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `length` | `listlike` | `num` |
| `length_each` | `listlike.list` | `num.list` |

<br/>
{{ macros.load_cut(user, 'examples/listlike.cut') }}
</div>
<div class="moduleblock">
<h3>PsiBLAST module</h3>

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

| Type      | Meaning |
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

<br/>
{{ macros.load_cut(user, 'examples/psiblast.cut') }}
</div>
<div class="moduleblock">
<h3>CRB-BLAST module</h3>

Conditional reciprocal BLAST best hits (Aubry et al. 2014).

Types:

| Type      | Meaning |
| :-------- | :------ |
| `fna` | FASTA (nucleic acid) |
| `faa` | FASTA (amino acid) |
| `fa` | FASTA (nucleic OR amino acid) |
| `crb` | tab-separated table of conditional reciprocal blast best hits |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `crb_blast` | `fna`, `fa` | `crb` |
| `crb_blast_each` | `fna`, `fa.list` | `crb.list` |

<br/>
{{ macros.load_cut(user, 'examples/crbblast.cut') }}
</div>
<div class="moduleblock">
<h3>HMMER module</h3>

Search sequences with hidden Markov models.

Types:

| Type      | Meaning |
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

<br/>
{{ macros.load_cut(user, 'examples/hmmer.cut') }}
</div>
<div class="moduleblock">
<h3>BlastRBH module</h3>

Reciprocal BLAST+ best hits.

Types:

| Type      | Meaning |
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

<br/>
{{ macros.load_cut(user, 'examples/blastrbh.cut') }}
</div>
<div class="moduleblock">
<h3>MUSCLE module</h3>

Align sequences with MUSCLE.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `aln` | multiple sequence alignment |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `muscle` | `faa` | `aln` |
| `muscle_each` | `faa.list` | `aln.list` |

<br/>
{{ macros.load_cut(user, 'examples/muscle.cut') }}
</div>
<div class="moduleblock">
<h3>Sample module</h3>

Random (but reproducable) sampling of list elements.

WARNING: Because of the way ShortCut caches tempfiles, calling these
more than once will give the same sublist each time! For different
sublists, use in combination with the 'repeat' function.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `sample` | `num`, `X.list` | `X.list` |

<br/>
{{ macros.load_cut(user, 'examples/sample.cut') }}
</div>
<div class="moduleblock">
<h3>Permute module</h3>

Generate random permutations of lists.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `leave_each_out` | `X.list` | `X.list.list` |

<br/>
{{ macros.load_cut(user, 'examples/permute.cut') }}
</div>
<div class="moduleblock">
<h3>Summarize module</h3>

Collapse a list of results into a single summary.



<br/>
{{ macros.load_cut(user, 'examples/summarize.cut') }}
</div>
<div class="moduleblock">
<h3>Scores module</h3>

Score repeated variables for plotting.


Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `score_repeats` | `<outputnum>`, `<inputvar>`, `<inputlist>` | `<input>.scores` |
| `extract_scores` | `X.scores` | `num.list` |
| `extract_scored` | `X.scores` | `X.list` |

<br/>
{{ macros.load_cut(user, 'examples/scores.cut') }}
</div>
<div class="moduleblock">
<h3>Plots module</h3>

Generate half-decent plots.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `png` | png image of a plot |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `histogram` | `str`, `num.list` | `plot` |
| `linegraph` | `str`, `num.scores` | `plot` |
| `scatterplot` | `str`, `num.scores` | `plot` |

<br/>
{{ macros.load_cut(user, 'examples/plots.cut') }}
</div>
<div class="moduleblock">
<h3>OrthoFinder module</h3>

Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `ofr` | OrthoFinder results |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `orthofinder` | `faa.list` | `ofr` |

<br/>
{{ macros.load_cut(user, 'examples/orthofinder.cut') }}
</div>
<div class="moduleblock">
<h3>Diamond module</h3>

Accelerated BLAST compatible local sequence aligner..

Types:

| Type      | Meaning |
| :-------- | :------ |
| `fna` | FASTA (nucleic acid) |
| `faa` | FASTA (amino acid) |
| `dmnd` | DIAMOND database |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `diamond_makedb` | `faa` | `dmnd` |
| `diamond_makedb_all` | `faa.list` | `dmnd` |
| `diamond_blastp` | `num`, `faa`, `faa` | `bht` |
| `diamond_blastp_sensitive` | `num`, `faa`, `faa` | `bht` |
| `diamond_blastp_more_sensitive` | `num`, `faa`, `faa` | `bht` |
| `diamond_blastp_db` | `num`, `faa`, `dmnd` | `bht` |
| `diamond_blastp_db_sensitive` | `num`, `faa`, `dmnd` | `bht` |
| `diamond_blastp_db_more_sensitive` | `num`, `faa`, `dmnd` | `bht` |
| `diamond_blastx` | `num`, `fna`, `faa` | `bht` |
| `diamond_blastx_sensitive` | `num`, `fna`, `faa` | `bht` |
| `diamond_blastx_more_sensitive` | `num`, `fna`, `faa` | `bht` |
| `diamond_blastx_db` | `num`, `fna`, `dmnd` | `bht` |
| `diamond_blastx_db_sensitive` | `num`, `fna`, `dmnd` | `bht` |
| `diamond_blastx_db_more_sensitive` | `num`, `fna`, `dmnd` | `bht` |

<br/>
{{ macros.load_cut(user, 'examples/diamond.cut') }}
</div>
<div class="moduleblock">
<h3>MMSeqs module</h3>

Many-against-many sequence searching: ultra fast and sensitive search and clustering suite.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `fna` | FASTA (nucleic acid) |
| `bht` | tab-separated table of blast hits (outfmt 6) |
| `mms` | MMSeqs2 sequence database |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `mmseqs_createdb_all` | `fa.list` | `mms` |
| `mmseqs_createdb` | `fa` | `mms` |
| `mmseqs_search_db` | `num`, `fa`, `mms` | `bht` |
| `mmseqs_search` | `num`, `fa`, `fa` | `bht` |

<br/>
{{ macros.load_cut(user, 'examples/mmseqs.cut') }}
</div>
<div class="moduleblock">
<h3>SonicParanoid module</h3>

Very fast, accurate, and easy orthology..

Types:

| Type      | Meaning |
| :-------- | :------ |
| `faa` | FASTA (amino acid) |
| `fna` | FASTA (nucleic acid) |
| `spr` | SonicParanoid results |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `sonicparanoid` | `faa.list` | `spr` |

<br/>
{{ macros.load_cut(user, 'examples/sonicparanoid.cut') }}
</div>
<div class="moduleblock">
<h3>OrthoGroups module</h3>

Common interface for working with the results of OrthoFinder, SonicParanoid, etc..

Types:

| Type      | Meaning |
| :-------- | :------ |
| `og` | orthogroups (orthofinder or sonicparanoid results) |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `orthogroups` | `og` | `str.list.list` |
| `orthogroup_containing` | `og`, `str` | `str.list` |
| `orthogroups_containing` | `og`, `str.list` | `str.list.list` |
| `ortholog_in_any` | `spr`, `faa.list` | `str.list.list` |
| `ortholog_in_any_str` | `str.list.list`, `str.list.list` | `str.list.list` |
| `ortholog_in_all` | `spr`, `faa.list` | `str.list.list` |
| `ortholog_in_all_str` | `str.list.list`, `str.list.list` | `str.list.list` |

<br/>
{{ macros.load_cut(user, 'examples/orthogroups.cut') }}
</div>
<div class="moduleblock">
<h3>Busco module</h3>

Benchmarking Universal Single-Copy Orthologs.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `blh` | BUSCO lineage HMMs |
| `bsr` | BUSCO results |
| `bst` | BUSCO scores table |
| `faa` | FASTA (amino acid) |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `load_lineage` | `str` | `blh` |
| `busco_list_lineages` | `str` | `str.list` |
| `busco_fetch_lineage` | `str` | `blh` |
| `busco_proteins` | `blh`, `faa` | `bsr` |
| `busco_proteins_each` | `blh`, `faa.list` | `bsr.list` |
| `busco_transcriptome` | `blh`, `fna` | `bsr` |
| `busco_transcriptome_each` | `blh`, `fna.list` | `bsr.list` |
| `busco_percent_complete` | `bsr` | `num` |
| `busco_percent_complete_each` | `bsr.list` | `num.list` |
| `busco_scores_table` | `bsr.list` | `bst` |
| `busco_filter_completeness` | `num`, `bst`, `faa.list` | `faa.list` |
| `concat_bst` | `bst.list` | `bst` |

<br/>
{{ macros.load_cut(user, 'examples/busco.cut') }}
</div>
<div class="moduleblock">
<h3>Range module</h3>

Generate ranges of numbers.

Types:

| Type      | Meaning |
| :-------- | :------ |
| `num` | number in scientific notation |

Functions:

| Name | Inputs | Output |
| :--- | :----- | :----- |
| `range_add` | `num`, `num`, `num` | `num.list` |
| `range_exponent` | `num`, `num`, `num`, `num` | `num.list` |
| `range_integers` | `num`, `num` | `num.list` |
| `range_length` | `num`, `num`, `num` | `num.list` |
| `range_multiply` | `num`, `num`, `num` | `num.list` |

<br/>
{{ macros.load_cut(user, 'examples/range.cut') }}
</div>
