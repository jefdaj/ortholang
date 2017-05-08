#!/usr/bin/env Rscript

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

download_matches <- function(tmpDir, fnName, schTable)
  # Calls biomartr and collects the output files
  apply(schTable, 1, function(row)
    getGenome(row["organism"], db=row["database"])
  )

save_list <- function(files, outPath) {
  # TODO can this be made generic? (later)
  cat(paste("save_list:", files, outPath, "\n"))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  tmpDir   <- args[[1]]
  outPath  <- args[[2]]
  fnName   <- readLines(args[[3]])
  schTable <- read.table(args[[4]], sep="\t", header=TRUE) 

  print(tmpDir)
  print(outPath)
  print(fnName)
  print(schTable)

  # assert_single_matches(fnName, schTable)
  # download_matches(tmpDir, fnName, schTable) %>% save_list(outPath)
  assert_single_matches(schTable)
  download_matches(tmpDir, fnName, schTable)
}

main()
