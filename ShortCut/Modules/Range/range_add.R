#!/usr/bin/env Rscript

seq_add <- function(start, stop, step)
  seq(start, stop, by=step)

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
  step  <- as.numeric(args[[4]])
  nums  <- seq_add(start, stop, step)
  save_list(path, nums)
}

main()
