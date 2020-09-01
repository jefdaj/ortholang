#!/usr/bin/env Rscript

read_num <- function(filename)
  scan(filename, what=numeric(), quiet=T)

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
  start  <- read_num(args[[2]])
  stop   <- read_num(args[[3]])
  length <- read_num(args[[4]])
	nums   <- range_length(start, stop, length)
  save_list(path, nums)
}

main()
