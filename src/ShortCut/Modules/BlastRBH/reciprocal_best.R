#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))

read_hits <- function(filename) {
  # read a table of BLAST hits from a file
  # should be a CSV formatted with the BLAST+ "-outfmt 6" option,
  # TODO move to a separate utilities file?
  # TODO leave out the colnames since we only use evalue?
  # cat(paste0('reading ', filename, '\n'))
  read.csv(filename, colClasses="character", sep="", quote="\n", col.names=c(
    'queryid', 'subjectid', 'percentidentity', 'alignmentlength',
    'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
    'bitscore')) %>% tbl_df
}

write_hits <- function(hits, filename)
  # TODO move to a separate utilities file?
  write.table(hits, filename, sep="\t",
              quote=FALSE, row.names=FALSE, col.names=FALSE)

# best_hits <- function(out, bht) {
#   print(head(bht))
#   read_hits(bht) %>%
#     arrange(evalue) %>%
#     group_by(queryid) %>%
#     filter(n() == 1) %>%
#     ungroup() %>%
#     write_hits(out)
# }

# TODO do this the proper way in its own cut function, man!
best_hits <- function(df)
  df %>%
    arrange(evalue) %>%
    group_by(queryid) %>%
    filter(n() == 1) %>%
    ungroup

reciprocal <- function(out, left, right) {
  # This should take a left and right best hits table,
  # and return the left table filtered for (query, subject) pairs
  # where the same two appear (reversed) in the right table.
  rightPairs <- read_hits(right) %>%
    best_hits %>%
    select(queryid=subjectid, subjectid=queryid) %>%
    distinct # TODO aren't they distinct already?
  res <- read_hits(left) %>%
    best_hits %>%
    semi_join(rightPairs, by=c('queryid', 'subjectid')) %>%
    write_hits(out)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  reciprocal(args[[1]], args[[2]], args[[3]])
}

main()
