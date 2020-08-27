#!/usr/bin/env Rscript

# TODO for consistency, should all scripts recieve the names?
# TODO pass env vars to run_script

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
	# cat('\n\n')

	# This part is a little weird. First you have to get the actual R args like this:
  args <- commandArgs(trailingOnly = TRUE)

	# There will always be args: the output path first, then the input path. 
  # The output path is directly usable:
  plotPath <- fix_ol_path(args[[1]])
	# cat(paste0('plotPath: ', plotPath, '\n'))

	# cat(paste0('args 2: ', args[[2]], '\n'))
  set_names <- read.list(args[[2]]) # %>% sapply(read.list)
	# cat(paste0('names: ', ns, '\n'))

	# The input path is the path to the list you gave the run_script function.
	# So you read that to get the list of arguments you really wanted:
	# cat(paste0('args 3: ', args[[3]], '\n'))
	sets <- read.list(args[[3]]) %>% lapply(read.num.list)
	# cat(paste0('sets: ', sets, '\n'))

	# In our case there's only one, corresponding to the `lol` variable
  # listPaths <- read.list(args_ol) %>% lapply(fix_ol_path)
	# cat(paste0('listPaths: ', listPaths, '\n'))
	
	# it's a num.list.list. here's how we can read it:
  # vennsets <- lapply(sets, read.num.list)

	# and here's how we can add the set names based on ortholang variables:
  names(sets) <- set_names
	# print(vennsets)

	# finally, we plot a venn diagram and save it to the plotPath
	plot_venn_diagram(sets, plotPath)
}

main()
