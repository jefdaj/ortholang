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

write_hits <- function(hits, filename)
  # TODO move to a separate utilities file?
  write.table(hits, filename, sep="\t",
              quote=FALSE, row.names=FALSE, col.names=FALSE)

filter_evalue <- function(out, num, bht) {
  cutoff <- as.numeric(read.table(num))
  read_hits(bht) %>% filter(evalue <= cutoff) %>% write_hits(out)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  filter_evalue(args[[1]], args[[2]], args[[3]])
}

main()
