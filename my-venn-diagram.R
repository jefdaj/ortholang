#!/usr/bin/env Rscript

# TODO should these have titles too? ask people
# TODO what if no varnames? i guess name by the cachedlines basename? handle that in haskell?

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))

# turn off the .log files
# from https://stackoverflow.com/a/36812214
suppressPackageStartupMessages(require(futile.logger))
invisible(futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger"))

# begin library #

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

# combined example of reading a num.list.list:
# first read the outer list to a character vector,
# then for each element read the inner num.list
# read.str.list('$TMPDIR/vars/lol.num.list.list') %>% lapply(read.num.list)

# end library #

#read_list <- function(filename) {
#  # read one list
#  print(filename)
#  filename <- gsub('\\$TMPDIR', '/root/.ortholang', filename)
#  scan(filename, what=character(), quiet=TRUE)
#}

fix_unnamed <- function(listnames) {
  # number any unnamed lists to differentiate them better
  listnames2 <- character(length(character()))
  for(i in seq_along(listnames)) {
    if (listnames[i] == "result") {
      listnames2[i] <- paste0("list", i)
    } else {
      listnames2[i] <- listnames[i]
    }
  }
	return(listnames2)
}

read_lol <- function(filename) {
  # read a list of lists
  filenames <- read.str.list(filename)
  lapply(setNames(filenames, sub('.txt', '', filenames)), read.str.list)
}

read_named_lol <- function(namesfile, listsfile) {
  print(namesfile)
  print(listsfile)
  listnames <- read.str.list(namesfile) %>% fix_unnamed
  print(listnames)
  lists <- read_lol(listsfile)
  print(lists)
  stopifnot(length(listnames) == length(lists))
  names(lists) <- listnames
  return(lists)
}

plot_lists <- function(lists, filename) {
  # The best way I can think to do this with no interaction is to draw the kind
  # of Venn diagrams people expect with up to 5 sets, and switch to upset for more.
  nlists <- length(lists)
  png(filename=filename, width=600, height=600)
  if (nlists > 5) {
    suppressPackageStartupMessages(require(UpSetR))
    p <- upset(fromList(lists))
    print(p) # for some reason, "print"ing it is necessary
  } else {
    suppressPackageStartupMessages(require(VennDiagram))
    # adding 2 makes it a nice green/blue/purple theme
    v <- venn.diagram(lists, fill = 2+(1:nlists), alpha=0.5, filename=NULL)
    # viewport prevents labels getting cut off
    # from https://stackoverflow.com/a/22826211
    grid.newpage()
    pushViewport(viewport(width=unit(0.95, "npc"), height = unit(1, "npc")))
    grid.draw(v)
  }
  invisible(dev.off())
}

main <- function() {
  # args <- c('testplot.png', 'testnames.txt', 'testlists.txt')
  args <- commandArgs(trailingOnly = TRUE)
  plotPath  <- args[[1]]

	# TODO for consistency, should all scripts recieve the names?
  # namesPath <- args[[2]] # might contain "<<emptystr>>"?
  namesPath <- '/home/jefdaj/ortholang/names.txt'

  listsPath <- args[[2]] # might contain "<<emptystr>>"?
  read_named_lol(namesPath, listsPath) %>% plot_lists(plotPath)
}

main()
