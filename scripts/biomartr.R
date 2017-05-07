#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(biomartr))
suppressPackageStartupMessages(require(dplyr))

assert_single_matches <- function(tmpdir, fnname, search) {
	# Checks that each row in the search table has a single match
	# (does this work with things besides genomes?)
	cat(paste(c("assert_single:", tmpdir, fnname, search)))
}

download_matches <- function(tmpdir, fnname, search) {
	# Calls biomartr and collects the output files
	cat(paste("download_matches:", tmpdir, fnname, search))
}

save_list <- function(files, outpath) {
	# TODO can this be made generic? (later)
	cat(paste("save_list:", files, outpath))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
	tmpdir  <- args[[1]] # tmp directory (shared across biomartr calls)
	outpath <- args[[2]] # where to write the list of downloaded files
	# restype <- args[[3]] # genome or proteome (cds, gff later?)
	fnname  <- args[[3]] # name of the function to call (getGenome etc)
	stable  <- args[[4]] # search table (species, optional db, optional id)
  assert_single_matches(tmpdir, fnname, search)
	# download_matches(tmpdir, fnname, search) %>% save_list(outpath)
}

main()
