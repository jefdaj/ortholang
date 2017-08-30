#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))

read_hits <- function(filename)
  # read a table of BLAST hits from a file
  # should be a CSV formatted with the BLAST+ "-outfmt 10" option,
  # prepended with an extra column for dbname
  # TODO move to a separate utilities file?
  # TODO only read the columns we use?
	# TODO is there really still a dbname?
  tbl_df(read.csv(filename, col.names=c(
    'queryid', 'subjectid', 'percentidentity', 'alignmentlength',
    'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
    'bitscore')))

best_hits <- function(out, bht) {
	# TODO is there still a dbname col?
	# TODO arrange does ascending by default right?
  read_hits(bht) %>% arrange(evalue) %>%distinct(queryid) %>% write(out)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  best_hits(args[[1]], args[[2]])
}

main()
