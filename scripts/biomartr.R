#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(biomartr))
suppressPackageStartupMessages(require(dplyr))

assert_single_matches <- function(fnName, schTable) {
	# Checks that each row in the search table has a single match
	# (does this work with things besides genomes?)
	cat(paste(c("assert_single:", fnName, schTable, "\n")))
}

download_matches <- function(tmpDir, fnName, search) {
	# Calls biomartr and collects the output files
	cat(paste("download_matches:", tmpDir, fnName, search, "\n"))
  return(list(paste(c(tmpDir, "fakematch.fna.gz"))))
}

save_list <- function(files, outPath) {
	# TODO can this be made generic? (later)
	cat(paste("save_list:", files, outPath, "\n"))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

	tmpDir   <- args[[1]] # tmp directory (shared across biomartr calls)
	outPath  <- args[[2]] # where to write the list of downloaded files
	fnName   <- args[[3]] # name of the function to call (getGenome etc)
	schTable <- args[[4]] # search table (species, optional db, optional id)

	cat(paste(c("tmpDir:"  , tmpDir  , "\n")))
	cat(paste(c("outPath:" , outPath , "\n"))) # TODO multiple instead?
	cat(paste(c("fnName:"  , fnName  , "\n")))
	cat(paste(c("schTable:", schTable, "\n"))) # TODO fix cache thing

  # assert_single_matches(fnName, schTable)
	# download_matches(tmpDir, fnName, search) %>% save_list(outPath)
}

main()
