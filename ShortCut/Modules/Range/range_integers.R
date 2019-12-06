#!/usr/bin/env Rscript

range_integers <- function(start, stop)
  seq.int(start, stop)

save_list <- function(outPath, elems) {
	con <- file(outPath)
  if (length(elems) == 0) {
      elems <- "<<emptylist>>"
  }
  writeLines(as.character(elems), con)
	close(con)
}

main <- function() {
  args  <- commandArgs(trailingOnly = TRUE)
  path  <- args[[1]]
  start <- as.numeric(args[[2]])
  stop  <- as.numeric(args[[3]])
	nums  <- range_integers(start, stop)
  save_list(path, nums)
}

main()
