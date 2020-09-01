#!/usr/bin/env Rscript

read_num <- function(filename)
  scan(filename, what=numeric(), quiet=T)

range_exponent <- function(base, exp_start, exp_stop, exp_step)
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
  base  <- read_num(args[[2]])
  start <- read_num(args[[3]])
  stop  <- read_num(args[[4]])
  step  <- read_num(args[[5]])
	nums  <- range_exponent(base, start, stop, step)
  save_list(path, nums)
}

main()
