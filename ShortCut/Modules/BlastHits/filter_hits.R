#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))

read_hits <- function(filename)
  # read a table of BLAST hits from a file
  # should be a CSV formatted with the BLAST+ "-outfmt 6" option,
  # TODO move to a separate utilities file?
  # TODO leave out the colnames since we only use evalue?
  tbl_df(read.table(filename, sep="\t", col.names=c(
    'queryid', 'subjectid', 'percentidentity', 'alignmentlength',
    'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
    'bitscore')))

write_hits <- function(hits, filename) {
  # TODO move to a separate utilities file?
  if (length(hits) == 0) {
      hits <- "<<emptybht>>" # TODO <<emptylist>>?
  }
  write.table(hits, filename, sep="\t",
              quote=FALSE, row.names=FALSE, col.names=FALSE)
}

filter_hits <- function(out, colname, num, bht) {
  cutoff <- as.numeric(read.table(num)) # TODO fail if this doesn't parse!
  read_hits(bht) %>% filter_(colname <= cutoff) %>% write_hits(out) # TODO is that right?
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  filter_hits(args[[1]], args[[2]], args[[3]], args[[4]])
}

main()
