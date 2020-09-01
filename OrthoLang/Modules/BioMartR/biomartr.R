#!/usr/bin/env Rscript

# TODO: cache results in ortholang to prevent wasting time on FTP stuff
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

# TODO ref and non-ref versions to get around the annoying different arguments

assert_single_matches <- function(schTable) {
  # Checks that each row in the search table has a single match
  apply(schTable, 1, function(row) stopifnot(
    nrow(is.genome.available(db=row["database"], row["organism"], details=TRUE)) == 1
  ))
}

# TODO: try to get your hands on the 0.5 release before obsessing over errors!
#       in fact, don't do that at all until much later! just cache results for now
# TODO: retry a couple times when there's an FTP error
# TODO: another error to catch:
# ----------> No reference genome or representative genome was found for
# 'Synechococcus sp. strain PCC 7002'. Thus, download for this species has been
# omitted.

download_matches <- function(fnName, schTable, tmpDir) {
  # Calls biomartr and collects the output files

  # seems to be required for caching the ftp files
  setwd(tmpDir)

  # print(system("pwd"))
  files <- apply(schTable, 1, function(row) {
    # sink("/dev/null", type=c("output", "message"))
    # g <- capture.output(suppressMessages(
    # ))
    # sink()
    # cat(paste0(c("Got '", g, "'\n")))
    # return(g)
    # getGenome(row["organism"], db=row["database"], path=tmpDir)
    # do.call(fnName, list(row["organism"], db=row["database"], path=tmpDir))
    do.call(fnName, list(row["organism"], db=row["database"], reference=FALSE))
  })

  files <- file.path(tmpDir, files)
  return(files)
}

save_list <- function(files, outPath) {
  con <- file(outPath)
  if (length(files) == 0) {
      files <- "<<emptylist>>"
  }
  writeLines(files, con)
  close(con)
}

main <- function() {

  args     <- commandArgs(trailingOnly = TRUE)
  outPath  <- args[[1]]
  logPath  <- gsub('result$', 'log', outPath) # TODO use out/err instead?
  fnName   <- readLines(args[[2]])
  tmpDir <- file.path(Sys.getenv('TMPDIR'), "cache", "biomartr") # TODO pass directly?

  # con <- file(logPath, open="wt")
  # sink(con)
  # sink(con, type='message')

  schTable <- read.table(args[[3]], sep="\t", header=TRUE) 
  assert_single_matches(schTable)
  download_matches(fnName, schTable, tmpDir) %>% save_list(outPath)
}

main()
