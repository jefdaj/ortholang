#!/usr/bin/env Rscript

seq_exponent <- function(base, exp_start, exp_stop, exp_step)
  base^seq(exp_start, exp_stop, by=exp_step)

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
  base  <- as.numeric(args[[2]])
  start <- as.numeric(args[[3]])
  stop  <- as.numeric(args[[4]])
  step  <- as.numeric(args[[5]])
	nums  <- seq_exponent(base, start, stop, step)
  save_list(path, nums)
}

main()
