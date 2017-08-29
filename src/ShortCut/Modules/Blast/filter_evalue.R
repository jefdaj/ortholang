#!/usr/bin/env Rscript

# TODO update to outfmt 6

suppressPackageStartupMessages(require(dplyr))

read_hits <- function(filename)
  # read a table of BLAST hits from a file
  # should be a CSV formatted with the BLAST+ "-outfmt 10" option,
  # prepended with an extra column for dbname
  # TODO move to a separate utilities file?
  # TODO only read the columns we use?
  tbl_df(read.csv(filename, col.names=c(
    'queryid', 'subjectid', 'percentidentity', 'alignmentlength',
    'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
    'bitscore')))

filter_evalue <- function(out, num, bht) {
	read_hits(bht) %>% filter(evalue >= as.numeric(num)) %>% write(out)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  filter_evalue(args[[1]], args[[2]], args[[3]])
}

main()
