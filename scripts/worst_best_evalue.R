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

worst_best_evalue <- function(out_num, hit_table, gene_list) {
  # Find the best BLAST e-value for each known gene in each genome,
  # then return the worst (highest) of those.
  # Intended as an approximation of the cutoff that would have
  # re-discovered all your known genes.
  # Assumes all these genomes are known to actually have all these genes.
  genes <- readLines(gene_list)
  read_hits(hit_table) %>%
    filter(queryid %in% genes) %>%
    arrange(evalue) %>%
    distinct(dbname, queryid) %>%
    .[['evalue']] %>% max %>%
    write(out_num)
}

main <- function() {
  # first arg is a tmp dir, but we don't need it here
  args <- commandArgs(trailingOnly = TRUE)
  worst_best_evalue(args[[2]], args[[3]], args[[4]])
}

main()
