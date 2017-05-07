#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(biomartr))
suppressPackageStartupMessages(require(dplyr))

# read_hits <- function(filename)
#   # read a table of BLAST hits from a file
#   # should be a CSV formatted with the BLAST+ "-outfmt 10" option,
#   # prepended with an extra column for dbname
#   # TODO move to a separate utilities file?
#   # TODO only read the columns we use?
#   tbl_df(read.csv(filename, col.names=c(
#     'dbname', 'queryid', 'subjectid', 'percentidentity', 'alignmentlength',
#     'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
#     'bitscore')))
# 
# filter_genes <- function(out_genes, genome_list, hit_table, max_evalue) {
#   # TODO refactor this to extract the parts in common with filter_genomes
#   goms <- readLines(genome_list)
#   emax <- scan(max_evalue, quiet=TRUE)
#   read_hits(hit_table) %>%
#     filter(evalue <= emax) %>%
#     group_by(queryid) %>%
#     filter(all(goms %in% dbname)) %>%
#     .[['queryid']] %>% unique %>% as.character %>%
#     write(out_genes)
# }

assert_single_matches <- function(tmpdir, restype, search) {
	# Checks that each row in the search table has a single match
	# (does this work with things besides genomes?)
}

download_matches <- function(tmpdir, restype, search) {
	# Calls biomartr and collects the output files
}

save_list <- function(files, outpath) {
	# TODO can this be made generic? (later)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
	tmpdir  <- args[[1]] # tmp directory (shared across biomartr calls)
	outpath <- args[[2]] # where to write the list of downloaded files
	restype <- args[[3]] # genome or proteome (cds, gff later?)
	stable  <- args[[4]] # search table (species, optional db, optional id)
  assert_single_matches(tmpdir, restype, search)
	download_matches(tmpdir, restype, search) %>% save_list(outpath)
}

# main()
