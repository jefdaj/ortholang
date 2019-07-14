#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(data.table))

read_list <- function(filename)
  # read one list
  scan(filename, what=character(), quiet=TRUE)

read_hits <- function(filename) {
  # read a table of BLAST hits from a file
  # should be a CSV formatted with the BLAST+ "-outfmt 6" option,
  # TODO move to a separate utilities file?
  # TODO leave out the colnames since we only use evalue?
  # cat(paste0('reading ', filename, '\n'))
  read.csv(filename, colClasses="character", sep="", quote="\n", col.names=c(
    'queryid', 'subjectid', 'pident', 'alignmentlength',
    'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
    'bitscore')) %>% tbl_df
}

write_hits <- function(hits, filename) {
  # TODO move to a separate utilities file?
  if (length(hits) == 0) {
      hits <- "<<emptybht>>" # TODO <<emptylist>>?
  }
  write.table(hits, filename, sep="\t",
              quote=FALSE, row.names=FALSE, col.names=FALSE)
}

# TODO do this the proper way in its own dtr function, man!
best_hits <- function(df)
  df %>%
    arrange(evalue) %>%
    group_by(queryid) %>%
    filter(n() == 1) %>%
    ungroup

reciprocal_best <- function(pair) {
  # This should take a left and right best hits table,
  # and return the left table filtered for (query, subject) pairs
  # where the same two appear (reversed) in the right table.
	left  <- pair[['left']]
  right <- pair[['right']]
  rightPairs <- read_hits(right) %>%
    best_hits %>%
    select(queryid=subjectid, subjectid=queryid) %>%
    distinct # TODO aren't they distinct already?
  res <- read_hits(left) %>%
    best_hits %>%
    semi_join(rightPairs, by=c('queryid', 'subjectid'))
  return(res)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out_bht <- args[[1]]
  left_bhts  <- read_list(args[[2]])
  right_bhts <- read_list(args[[3]])
	stopifnot(length(left_bhts) == length(right_bhts))
  pairs <- mapply(function(l,r) list(left=l, right=r), left_bhts, right_bhts, SIMPLIFY=FALSE)
	dfs   <- lapply(pairs, reciprocal_best)
  df    <- rbindlist(dfs, use.names=FALSE)
  write_hits(df, out_bht)
}

main()
