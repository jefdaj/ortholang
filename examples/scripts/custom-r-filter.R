#!/usr/bin/env Rscript

# This is based on OrthoLang/Modules/BlastHits/filter_hits.R, which filters by
# e-value cutoff. Here we filter by alignment length + bitscore, just to show
# that you can customize the criteria.

# Most of this script is boilerplate--that is, code you can paste as-is into
# any similar script. The only part you would need to customize to make your
# own filter script is lines 53-54.

suppressPackageStartupMessages(require(dplyr))

# Used in the read functions below.
# You can re-use it in any custom R script.
fix_ol_path <- function(filename)
  gsub('\\$TMPDIR' , Sys.getenv('TMPDIR') ,
  gsub('\\$WORKDIR', Sys.getenv('WORKDIR'), filename))

# Because single strings are also vectors in R, this also works as `read.str`.
# You can re-use it in any custom R script.
# It also works for a list of anything other than str or num: str.list.list, bht.list, etc.
# In that case it returns raw paths that look something like '$TMPDIR/exprs/list/7d4fb3401b/result'.
read.list <- function(filename) {
  lst <- filename %>% fix_ol_path %>% scan(what=character(), quiet=TRUE)
  if (length(lst) == 1 && lst == '<<emptylist>>') {
	  return(c())
	} else {
	  return(lapply(lst, fix_ol_path))
	}
}

# This is copied from filter_hits.R
# You can re-use it in any custom R script.
read_hits <- function(filename)
  read.table(filename, sep="\t", col.names=c(
    'queryid', 'subjectid', 'pident', 'alignmentlength',
    'mismatches', 'gapopens', 'qstart', 'qend', 'sstart', 'send', 'evalue',
    'bitscore')) %>% tbl_df

# This is copied from filter_hits.R
# You can re-use it in any custom R script.
write_hits <- function(hits, filename) {
  if (length(hits) == 0) { hits <- "<<emptybht>>" }
  write.table(hits, filename, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE)
}

filter_hits <- function(inPath, outPath) {
	# Standard read step
	df <- read_hits(inPath)

  # Custom filter for alignment length + bitscore
  df <- df %>%
					mutate(alignmentlength=as.numeric(alignmentlength), bitscore=as.numeric(bitscore)) %>%
					filter(alignmentlength >= 300, bitscore >= 500)

	# Standard write step
  write_hits(df, outPath)
}

main <- function() {
	# This part is a little weird. First you have to get the actual R args like this:
  args <- commandArgs(trailingOnly = TRUE)

	# There will always be 3 of them from OrthoLang:
  #
  # 1. the final path where this script should write its result
  # 2. a list of variable names, which may be empty or incomplete depending on the orhtholang script
  # 3. a list of the corresponding variable paths
  outPath  <- fix_ol_path(args[[1]]) # TODO is fixing this one necessary?
  varNames <- read.list(args[[2]]) # not used in this script
	varPaths <- read.list(args[[3]])

	# In this case we only expect one input bht, and don't care about its name.
	# So we take the first var path, filter its contents, and write to the outPath.
	bhtPath <- varPaths[[1]]
	filter_hits(bhtPath, outPath)
}

main()
