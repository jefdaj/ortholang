#!/usr/bin/env Rscript

# TODO for consistency, should all scripts recieve the names?
# TODO pass env vars to run_script

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(VennDiagram))

# used in the read functions below
fix_ol_path <- function(filename)
  gsub('\\$TMPDIR' , Sys.getenv('TMPDIR') ,
  gsub('\\$WORKDIR', Sys.getenv('WORKDIR'), filename))

# Because single strings are also vectors in R, this also works as `read.str`.
# It also works for a list of anything other than str or num: str.list.list, bht.list, etc.
# In that case it returns raw paths that look something like '$TMPDIR/exprs/list/7d4fb3401b/result'.
read.str.list <- function(filename)
  filename %>% fix_ol_path %>% scan(what=character(), quiet=TRUE)

# because single numbers are also vectors in R, this also works as read.num
read.num.list <- function(filename)
  filename %>% fix_ol_path %>% scan(what=numeric(), quiet=TRUE)

plot_venn_diagram <- function(lists, filename) {
	# the default ortholang version switches to upset plots here:
	# but for this custom one we'll skip that and make bigger venn diagrams instead

  png(filename=filename, width=600, height=600)

  # adding 2 gives it the default green/blue/purple theme, so let's go with 3 here
  nlists <- length(lists)
  v <- venn.diagram(lists, fill = 3+(1:nlists), alpha=0.5, filename=NULL)

  # viewport prevents labels getting cut off
  # from https://stackoverflow.com/a/22826211
  grid.newpage()
  pushViewport(viewport(width=unit(0.95, "npc"), height = unit(1, "npc")))
  grid.draw(v)

  invisible(dev.off())
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  plotPath  <- args[[1]]
  # namesPath <- args[[2]]
  namesPath <- '/home/jefdaj/ortholang/names.txt'
  listsPath <- args[[3]]
	
	# reads a num.list.list and naming the inner lists by ortholang variables
  vennsets <- read.str.list(listsPath) %>% lapply(read.num.list)
  names(vennsets) <- read.str.list(namesPath)

	# plots a venn diagram and saves it to the plotPath
	plot_venn_diagram(vennsets, plotPath)
}

main()
