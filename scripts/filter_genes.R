#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))

# TODO maybe the columns are different now that it's not parsing seqids?

read_hits <- function(filename)
  # read a table of BLAST hits from a file
  # should be a CSV formatted with the BLAST+ "-outfmt 10" option,
  # prepended with an extra column for dbname
  # TODO move to a separate utilities file?
  # TODO only read the columns we use?
  tbl_df(read.csv(filename, col.names=c(
    'dbname', 'queryid', 'subjectid', 'percentidentity', 'alignmentlength',
    'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
    'bitscore')))

filter_genes <- function(out_genes, genome_list, hit_table, max_evalue) {
  # TODO refactor this to extract the parts in common with filter_genomes
  goms <- readLines(genome_list)
  emax <- scan(max_evalue, quiet=TRUE)
  read_hits(hit_table) %>%
    filter(evalue <= emax) %>%
    group_by(queryid) %>%
    filter(all(goms %in% dbname)) %>%
    .[['queryid']] %>% unique %>% as.character %>%
    write(out_genes)
}

main <- function() {
  # first arg is a tmp dir, but we don't need it here
  args <- commandArgs(trailingOnly = TRUE)
  filter_genes(args[[2]], args[[3]], args[[4]], args[[5]])
}

main()
