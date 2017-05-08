#!/usr/bin/env Rscript

# TODO: cache results in shortcut to prevent wasting time on FTP stuff
# TODO: hardcode args for development
# TODO: put downloads in tmpDir (will be created by Shake?)
# TODO: search specific databases and filter by identifier
#       (build up args as list, then use do.call?)
# TODO: nice messages when more than one genome/whatever found
# TODO: will this work with things other than genomes??

suppressPackageStartupMessages(require(biomartr))
suppressPackageStartupMessages(require(dplyr))

assert_single_matches <- function(schTable) {
  # Checks that each row in the search table has a single match
  apply(schTable, 1, function(row) stopifnot(
    nrow(is.genome.available(row["organism"], details=TRUE)) == 1
  ))
}

# TODO: try to get your hands on the 0.5 release before obsessing over errors!
#       in fact, don't do that at all until much later! just cache results for now
# TODO: retry a couple times when there's an FTP error
# TODO: another error to catch:
# ----------> No reference genome or representative genome was found for
# 'Synechococcus sp. strain PCC 7002'. Thus, download for this species has been
# omitted.

download_matches <- function(tmpDir, fnName, schTable)
  # Calls biomartr and collects the output files
  apply(schTable, 1, function(row) {
	  # sink("/dev/null", type=c("output", "message"))
    # g <- capture.output(suppressMessages(
		# ))
	  # sink()
		# cat(paste0(c("Got '", g, "'\n")))
		# return(g)
		getGenome(row["organism"], db=row["database"], path=tmpDir)
  })

save_list <- function(files, outPath) {
	con <- file(outPath)
  writeLines(files, con)
	close(con)
}

main <- function() {
  args     <- commandArgs(trailingOnly = TRUE)
  tmpDir   <- args[[1]]
  outPath  <- args[[2]]
  fnName   <- readLines(args[[3]])
  schTable <- read.table(args[[4]], sep="\t", header=TRUE) 
  assert_single_matches(schTable)
  download_matches(tmpDir, fnName, schTable) %>% save_list(outPath)
}

main()
