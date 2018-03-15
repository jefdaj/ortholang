ShortCut: short, reproducible phylogenomic cuts
===============================================

Phylogenomic cuts are lists of genes whose distribution suggests they may be
important for a trait of interest, such as virulence or drought tolerance. They
have historically been successful at identifying candidate genes for further
study, but no standard methodology exists for making them or measuring their
quality. They tend to involve a small number of tools--BLAST searches, set
subtraction, perhaps gene trees, and manual curation--combined in unique ways
depending on the organisms and traits involved. That makes the overall process
difficult to automate with a single program. ShortCut attempts to overcome that
using a domain-specific language. It provides simple functions that can be
rearranged to make a wide variety of cuts, as well as a novel method of
measuring their robustness to changes in the search parameters. Cut scripts are
reproducible, automatically parallelized, and facilitate quick comparison of
many possible mehods to arrive at a reliable list of candidate genes.

Note that this README is based on [the poster][1], and most of the
same information will also be available as a series of screencasts.

Build and Test
--------------

ShortCut is best built using [Nix][2], which ensures that all dependencies are
exactly satisfied. Not much human work is required, but it will download and/or
build a lot of packages and store them in `/nix`.

First you need the package manager itself. See [the website][2] for
instructions, or just run this:

    curl https://nixos.org/nix/install | sh

Next, clone this repository and run `nix-build -j$(nproc)` inside it. It will
eventually create a symlink called `result` that points to the finished
package.

Before using it, run the test suite to check that everything works:

    ./result/bin/shortcut --test

Run and edit scripts
--------------------

These commands will run an existing script, load an existing script in the
interpreter, and start a new script in the interpreter respectively:

* `shortcut --script your-existing.cut`
* `shortcut --script your-existing.cut --interactive`
* `shortcut`

See [usage.txt][5] for other command line options, and type `:help` in the
interpreter for a list of special commands. Note that unlike most languages, in
`shortcut` you can always save a valid script from your current interpreter
session with something like `:write your-updated.cut`.

Basics
------

You can load gene lists and genomes, and convert them between several common
formats:

```python
# load a FASTA nucleic acid file (transcriptome)
pcc6803 = load_fna "Synechocystis_PCC_6803.fna"

# load a genome distributed as separate genbank files,
# and convert it to one FASTA
pcc7942 = concat_fastas [gbk_to_faa (load_gbk "SynPCC7942_chr.gbk"),
                         gbk_to_faa (load_gbk "SynPCC7942_pANL.gbk")]
```

Another common task is to perform set operations on lists of things.
It works with genes and genomes, but is easier to demonstrate with numbers.
We can quickly describe anything that could be shown in a Venn Diagram:

<img src="https://github.com/jefdaj/ShortCut/raw/master/poster/venn-sets.png" width="300">

~~~
>> # For example, starting with these lists...
>> A = [1, 2, 3]
>> B = [2, 4, 6]
>> C = [45, 2, 1e23, 3e-9]
>> 
>> A | B
[1, 2, 3, 4, 6]
>> A & B
[2]
>> A ~ B
[1, 3]
>> all [A, B, C]
[2]
>> any [A, B, C]
[1, 1.0e23, 2, 3, 3.0e-9, 4, 45, 6]
>> B ~ (A & C)
[4, 6]
~~~

The type system will stop you from combining lists of different types, like
numbers and strings or strings and blast hit tables.

Evaluation mechanics
--------------------

While ShortCut feels like a scripting language, the way it runs commands is
more closely analogous to [Make][7]. (Actually, it uses [Shake][8].) When you
ask it to calculate something, it:

1. Maps each variable in the script to a filename like `<varname>.<type>`.
   For example `pcc6803.fna` or `cutoffs.num.list`.
2. Makes a graph of which files are used as inputs and outputs of each command
3. Runs the commands in whatever order is needed to "build" the variable you
   asked for

Most of the interesting properties of the language, like lazy evaluation and
automatic deduplication + parallelization, follow from this design. It also
lets you view or edit the result of any function call in another program if
needed. They're stored by default in `~/.shortcut`.

The special variable `result` automatically tracks your most recent expression.
You can also assign it expclicitly. It's the only thing that will actually be
evaluated when running a script.

BLAST+
------

ShortCut provides the most common NCBI BLAST programs, which differ in their
subject and query types.

| Function | Query | Subject |
| :------- | :---- | :------ |
| blastn   | nucl  | nucl    |
| blastp   | prot  | prot    |
| blastx   | trans | prot    |
| tblastn  | prot  | trans   |
| tblastx  | trans | trans   |

There are several variants of each one, named with suffixes:

| Format            | Meaning |
| :-----            | :------ |
| function          | "Regular" version (no suffix) automatically creates a database from the subject FASTA file before searching |
| function_db       | Uses a prebuilt BLAST database as the subject. Useful for searching the larger NCBI databases, such as nr or refseq_rna |
| function_rbh      | Does forward and reverse searches (query -> subject, subject -> query), and keeps only the reciprocal best hits (those where each gene is the other's top hit) |
| function_each     | BLASTs the query against a list of subjects and returns a list of hit tables |
| function_db_each  | Searches against a list of prebuilt databases |
| function_rbh_each | Reciprocal best hits against a list of FASTA files |

A couple examples:

```python
# BLAST against the NCBI nonredundant database
hits = tblastn 1e-50 pcc7942 (blastdbget "nr")

# find Synechococcus orthologs of Synechocystis genes
# (this lists the Synechococcus genes themselves;
#  for the Synechocystis genes that have orthologs, use extract_queries)
genes = extract_targets (blastx 1e-20 pcc6803 pcc7942)
```

CRB-BLAST
---------

Reciprocal best hits are the most common method used to find orthologs, but
they can sometimes be overly conservative, missing true orthologs. For that
reason, ShortCut also includes CRB-BLAST ([Aubry _et al._ 2014][4]). For each
pair of genomes, it:

1. Does a standard reciprocal BLAST search
2. Plots e-value vs sequence length of the reciprocal best hits and fits
   a curve to it
3. Adds non-reciprocal hits whose e-values are at least as good 

This is illustrated in the paper:

<img src="https://github.com/jefdaj/ShortCut/raw/master/poster/crb-blast.png" height="400">

According to the authors it significantly improves the accuracy of ortholog
assignment. Another useful feature is that it prevents having to pick e-value
cutoffs.

Example:

```python
# make a list of Chlamy genes with a reliable ortholog in at least one diatom
shared_with_diatoms = any (extract_queries_each (crb_blast_each chlamy diatoms))
```

Codifying the Greencut
----------------------

For a more complete example, consider the Greencut ([Merchant _et al._
2007][3]), a list of genes likely to be important for photosynthesis because
they are conserved in the "green lineage". From their paper, here are the
species used to make the original Greencut, along with related cuts for diatoms
and plastids:

<img src="https://github.com/jefdaj/ShortCut/raw/master/poster/greencut-species.jpg" width="500">

> Evolutionary relationships of 20 species with sequenced genomes used
> for the comparative analyses in [their] study include cyanobacteria and
> nonphotosynthetic eubacteria, Archaea and eukaryotes from the oomycetes,
> diatoms, rhodophytes, plants, amoebae and opisthokonts. 

To do something similar in ShortCut, a proteome (FASTA amino acid file) is
loaded for each species. They are grouped into lists representing the most
specific taxa, and higher taxa are represented by unions of those lists:

```python
cyanobacteria = load_faa_each
  [ "Synechocystis_sp_PCC-6803.faa"
  , "Prochlorococcus-marinus.faa"
  ]

other_bacteria = load_faa_each
  [ "Pseudomonas_aeruginosa.faa"
  , "Staphylococcus_aureus.faa"
  , "Mathanosarcina_acetovorans.faa"
  , "Sulfolobus_solfataricus.faa"
  ]

# JGI distributes the diatom genomes as a separate mapped and unmapped fasta
# file each, so we have to concatenate them to get whole genomes.
# (Alternately, we could ignore unmapped reads.)
ptr = concat_fastas (load_faa_each
  [ "Phaeodactylum_tricornatum_mapped.faa"
  , "Phaeodactylum_tricornatum_unmapped.faa"
  ])

tps = concat_fastas (load_faa_each
  [ "Thalassiosira_pseudonana_mapped.faa"
  , "Thalassiosira_pseudonana_unmapped.faa"
  ])

diatoms = [ptr, tps]

oomycetes = load_faa_each
  [ "Phytophthora_ramorum.faa"
  , "Phytophthora_sojae.faa"
  ]

# We separate Chlamydomonas from the other Chlorophytes because it will be
# used as the reference, and load it as DNA rather than AA sequences because
# CRB-BLAST requires it.
chlamy = load_fna "Chlamydomonas_reinhardtii.fna"

chlorophyta = load_faa_each
  [ "Ostreococcus_taurii.faa"
  , "Ostreococcus_lucimarinus.faa"
  ]

streptophyta = load_faa_each
  [ "Arabidopsis_thaliana.faa"
  , "Physcomitrella_patens.faa"
  ]

cmerolae = load_faa "Cyanidioschyzon_merolae.faa"

unikonts = load_faa_each
  [ "Dictyostelium_discoideum.faa"
  , "Homo_sapiens.faa"
  , "Neurospora_crassa.faa"
  , "Caenorhabditis_elegans.faa"
  ]

heterokonts   = diatoms | oomycetes
bacteria      = cyanobacteria | other_bacteria
viridiplantae = chlorophyta | streptophyta
plantae       = viridiplantae | [cmerolae]
eukaryotes    = plantae | heterokonts | unikonts
```

Next, here is an exerpt of how the authors describe the cuts themselves:

<img src="https://github.com/jefdaj/ShortCut/raw/master/poster/greencut-venn.jpg" width="400">

> The Greencut comprises 349 _Chlamydomonas_ proteins with homologs in
> representatives of the green lineage of the Plantae (_Chlamydomonas_,
> _Physcomitrella_, and _Ostreococcus tauri_ and _O. lucimarinus_), but not in
> nonphotosynthetic organisms. Genes encoding proteins of unknown function that
> were not previously annotated were given names on the basis of their occurrence
> in various cuts. CGL refers to conserved only in the green lineage. The
> Greencut protein families, which also include members from the red alga
> _Cyanidioschyzon_ within the Plantae, were assigned to the PlantCut (blue plus
> green rectangles). CPL refers to conserved in the Plantae. Greencut proteins
> also present in at least one diatom (_Thalassiosira_ and _Phaeodactylum_) were
> assigned to the DiatomCut (yellow plus green rectangle). CGLD refers to
> conserved in the green lineage and diatoms. Proteins present in all of the
> eukaryotic plastid-containing organisms in this analysis were assigned to the
> PlastidCut (green rectangle). CPLD refers to conserved in the Plantae and
> diatoms. The criteria used for the groupings associated with the Greencut are
> given in the lower table.

In roughly equivalent ShortCut code, two more groups (lists of proteomes)
are defined for convenience: "greens" and "others".

```python
greens = plantae | cyanobacteria
others = unikonts | heterokonts | other_bacteria
```

Then CRB-BLAST searches are done between Chlamydomonas and each of the other
genomes. Finally, the cuts are made by extracting the list of Chlamy genes
with homologs in each species, and doing set operations on them.

```python
green_hits  = extract_each_queries (crb_blast_each chlamy greens )
diatom_hits = extract_each_queries (crb_blast_each chlamy diatoms)
other_hits  = extract_each_queries (crb_blast_each chlamy others )

greencut   = all green_hits ~ any other_hits
plantcut   = greencut & extract_queries (crb_blast chlamy cmerolae)
diatomcut  = greencut & any diatom_hits
plastidcut = plantcut & diatomcut
```

Note that even if many duplicate BLAST searches are specified, caching ensures
each operation is only done once. It is also possible to calculate one cut at
a time, skipping any steps whose results aren't used in that one.

Permute, Repeat, Summarize
--------------------------

Making a cut involves choices: which genomes to include, whether to trust their
gene annotations, which BLAST functions to use, which e-value cutoffs to apply
at each step... How can you be sure the parameters you picked are reasonable?
ShortCut implements a novel solution made possible by lazy evaluation and
caching: duplicate parts of the program, re-run them starting from alternate
values, and see how the results change.

Suppose you have the original program in the box on the left, and want to know,
"What happens to `var6` if I change `var1`?"

<img src="https://github.com/jefdaj/ShortCut/raw/master/poster/prs.png" width="800">

`repeat_each` recalculates `var6` starting from 3 alternate versions of `var1`,
and reports a list of results.

Note that this is all "repeat"; the "permute" and "summarize" steps would be
separate functions to generate the list of `var1` permutations and aggregate
the final results in some way, perhaps filtering or plotting them. The overall
strategy is similar to "split apply combine" in R.

Here is a simpler and more practical example:

```python
# BLAST Synechococcus genes against Synechocystis with a standard cutoff
cutoff = 1e-10
hits = extract_queries (blastp cutoff pcc7942 pcc6803)

# Re-run it with a range of cutoffs and report the number of hits for each
cutoffs = [1e-5, 1e-10, 1e-20, 1e-50, 1e-100, 0]
lengths = repeat_each (length hits) cutoff cutoffs
```

By using different permute and summarize functions, many biologically relevant
questions can be easily tranlated to code:

* "How long does it take to run my cut with only a few of the available cyano
  genomes?"

* "Do I really need all those genomes, or would using just a couple produce the
  same results?"

* "If I remove the known PSII assembly factors one by one, which would be
  rediscovered in the results?"

* Which search parameters maximize the number of known PSII assembly factors
  (positive controls) rediscovered?

* "If I re-run the same BLAST search 1000 times, which hits always appear and
  which ones only show up occasionally?"

* ...

Even better, that code can be added interactively and without changing the
existing cut! It also works recursively, so you can repeat your 1000 BLAST runs
with different parameters, then use only the best version as input to the next
stage of the cut.

Next: Cross-Validation
----------------------

The PRS methodology will be extended to automatically optimize parameters
(questions 3 and 4 above). For example you could try a range of e-values and
pick the one the maximizes the number of known genes rediscovered while
minimizing the number of total candidates.

It is considered good practice when training an algorithm to hold some of the
test data in reserve for measuring performance at the end. This guards against
over-fitting: optimizing the algorithm for your exact test data, only to find
later that other data are different. This could also be done fairly easily in
ShortCut by splitting a list of known positive-control genes into training and
validation lists, optimizing to discover genes in the training list, and
finally reporting the number of validation genes rediscovered at the same time.

Finally, that could be refined into cross-validation: splitting the known genes
into some number of sublists and using each one as validation for the others.
For example:

1. start with 50 known genes
2. split into 5 random lists of 10
3. for each list of 10, train on the other 40 and test on those 10
4. plot the results

Next: build and cluster gene trees
----------------------------------

After finding a large list of initial candidates with BLAST, one good way to
narrow them down is by aligning each set of homologs and building gene trees
from them, then clustering the trees and picking out the genes that cluster
with your positive controls. TreeCl ([Gori, Dessimoz _et al._ 2016][6])
automates much of the process:

<img src="https://github.com/jefdaj/ShortCut/raw/master/poster/treecl.jpeg" width="800">

Integrating it with ShortCut will improve them both, since TreeCl offers
a large number of tree building and clustering parameters that would benefit
from comparison using the PRS functions.

[1]: https://github.com/jefdaj/ShortCut/blob/master/poster/shortcut-poster.pdf
[2]: https://nixos.org/nix/
[3]: http://science.sciencemag.org/content/318/5848/245.full
[4]: http://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1004365
[5]: src/usage.txt
[6]: https://academic.oup.com/mbe/article/33/6/1590/2579727
[7]: https://en.wikipedia.org/wiki/Make_(software)
[8]: https://shakebuild.com/
