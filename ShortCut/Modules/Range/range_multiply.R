#!/usr/bin/env Rscript

# TODO guard this one against the factor having a negative sign

seq_multiply <- function(start, stop, factor) {
  current <- start
  accumulated <- start
  while (current * factor <= stop) {
    current <- current * factor
	  accumulated <- c(accumulated, current)
	}
	return(accumulated)
}

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
  factor <- as.numeric(args[[4]])
	nums   <- seq_multiply(start, stop, factor)
  save_list(path, nums)
}

main()
