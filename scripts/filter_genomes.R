#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))

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

filter_genomes <- function(out_genomes, gene_list, hit_table, max_evalue) {
  # TODO refactor this to extract the parts in common with filter_genes
  genes <- readLines(gene_list)
  emax  <- scan(max_evalue, quiet=TRUE)
  read_hits(hit_table) %>%
    filter(evalue <= emax) %>%
    group_by(dbname) %>%
    filter(all(genes %in% queryid)) %>%
    .[['dbname']] %>% unique %>% as.character %>%
    write(out_genomes)
}

main <- function() {
  # first arg is a tmp dir, but we don't need it here
  args <- commandArgs(trailingOnly = TRUE)
  filter_genomes(args[[2]], args[[3]], args[[4]], args[[5]])
}

main()
