#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(VennDiagram))

# turn off the .log files
# from https://stackoverflow.com/a/36812214
# TODO allow them, but make sure they go in the exprs/run_script_explicit dirs?
suppressPackageStartupMessages(require(futile.logger))
invisible(futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger"))

# used in the read functions below
fix_ol_path <- function(filename)
  gsub('\\$TMPDIR' , Sys.getenv('TMPDIR') ,
  gsub('\\$WORKDIR', Sys.getenv('WORKDIR'), filename))

# Because single strings are also vectors in R, this also works as `read.str`.
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

# because single numbers are also vectors in R, this also works as read.num
read.num.list <- function(filename)
  read.list(filename) %>% as.numeric

plot_venn_diagram <- function(lists, filename) {
	# the default ortholang version switches to upset plots for >5 lists,
	# but for this custom one we'll skip that and make bigger venn diagrams instead

  png(filename=filename, width=600, height=600)

  # adding 2 gives it the default green/blue/purple theme, so let's go with something else (9) here
  nlists <- length(lists)
  v <- venn.diagram(lists, fill = 9+(1:nlists), alpha=0.5, filename=NULL)

  # viewport prevents labels getting cut off
  # from https://stackoverflow.com/a/22826211
  grid.newpage()
  pushViewport(viewport(width=unit(0.95, "npc"), height = unit(1, "npc")))
  grid.draw(v)

  invisible(dev.off())
}

main <- function() {
	# This part is a little weird. First you have to get the actual R args like this:
  args <- commandArgs(trailingOnly = TRUE)

	# There will always be 3 of them:
  #
  # 1. the final path where this script should write its result
  # 2. a list of variable names, which may be empty or incomplete depending on the orhtholang script
  # 3. a list of the corresponding variable values

  # The output path is directly usable:
  plotPath <- fix_ol_path(args[[1]])

	# So are the variable names, although depending on the script you may not need them:
  set_names <- read.list(args[[2]])

	# How you should parse the variable paths depends on what type of data you sent to the script.
	# In this example they're a list of lists of numbers, and we want to make a Venn diagram
	# of how many numbers overlap between the lists. So there are 3 steps:
	#
	# 1. read the arguments file, which gives a list of list files
	# 2. for each list file, read it as a list of numbers
	# 3. name the lists by their ortholang variable names

	# steps 1-2
	sets <- read.list(args[[3]]) %>% lapply(read.num.list)

	# step 3
  names(sets) <- set_names

	# finally, we plot a venn diagram and save it to the plotPath
	plot_venn_diagram(sets, plotPath)
}

main()
