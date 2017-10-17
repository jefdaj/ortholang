#!/usr/bin/env Rscript

# TODO: cache results in shortcut to prevent wasting time on FTP stuff
# TODO: hardcode args for development

# TODO: put downloads in tmpDirs specific to the functions (genome, proteome, ...)

# TODO: search specific databases and filter by identifier
#       (build up args as list, then use do.call?)

# TODO: convert "more than one genome" warning to an error like this:
# http://stackoverflow.com/questions/8217901/breaking-loop-when-warnings-appear-in-r

# TODO: handle getProteome, getCDS, getGFF in addition to getGenome
# TODO: figure out why curl/whatever is spamming terminal and cut it off
#       (might be better done in haskell)

# TODO: haskell function to show the doc_ files when you print a variable,
#       and one to shorten it to one line for lists
#       ... in fact, you want general "show" and "show one line" functions

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

download_matches <- function(fnName, schTable)
  # Calls biomartr and collects the output files
  # print(system("pwd"))
  apply(schTable, 1, function(row) {
	  # sink("/dev/null", type=c("output", "message"))
    # g <- capture.output(suppressMessages(
		# ))
	  # sink()
		# cat(paste0(c("Got '", g, "'\n")))
		# return(g)
		# getGenome(row["organism"], db=row["database"], path=tmpDir)
		# do.call(fnName, list(row["organism"], db=row["database"], path=tmpDir))
		do.call(fnName, list(row["organism"], db=row["database"]))
  })

save_list <- function(files, outPath) {
	con <- file(outPath)
  writeLines(files, con)
	close(con)
}

main <- function() {
  args     <- commandArgs(trailingOnly = TRUE)
  # tmpDir   <- args[[1]]
  outPath  <- args[[1]]
  fnName   <- readLines(args[[2]])
  schTable <- read.table(args[[3]], sep="\t", header=TRUE) 
  assert_single_matches(schTable)
  download_matches(fnName, schTable) %>% save_list(outPath)
}

main()
