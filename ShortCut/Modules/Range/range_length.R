#!/usr/bin/env Rscript

range_length <- function(start, stop, length)
  seq(start, stop, length.out=length)

save_list <- function(outPath, elems) {
	con <- file(outPath)
  if (length(elems) == 0) {
      elems <- "<<emptylist>>"
  }
  writeLines(as.character(elems), con)
	close(con)
}

main <- function() {
  args   <- commandArgs(trailingOnly = TRUE)
  path   <- args[[1]]
  start  <- as.numeric(args[[2]])
  stop   <- as.numeric(args[[3]])
  length <- as.numeric(args[[4]])
	nums   <- range_length(start, stop, length)
  save_list(path, nums)
}

main()
