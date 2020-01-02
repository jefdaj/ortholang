#!/usr/bin/env Rscript

# TODO should these have titles too? ask people
# TODO what if no varnames? i guess name by the cachedlines basename? handle that in haskell?

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))

# turn off the .log files
# from https://stackoverflow.com/a/36812214
suppressPackageStartupMessages(require(futile.logger))
invisible(futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger"))

read_list <- function(filename)
  # read one list
  scan(filename, what=character(), quiet=TRUE)

read_lol <- function(filename) {
  # read a list of lists
  filenames <- read_list(filename)
  lapply(setNames(filenames, sub('.txt', '', filenames)), read_list)
}

read_named_lol <- function(namesfile, listsfile) {
  listnames <- read_list(namesfile)
  lists <- read_lol(listsfile)
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
    upset(fromList(lists))
  } else {
    suppressPackageStartupMessages(require(VennDiagram))
    # adding 2 makes it a nice green/blue/purple theme
    v <- venn.diagram(lists, fill = 2+(1:nlists), alpha=0.5, filename=NULL)
    # viewport prevents labels getting cut off
    # from https://stackoverflow.com/a/22826211
    grid.newpage()
    pushViewport(viewport(width=unit(0.7, "npc"), height = unit(0.7, "npc")))
    grid.draw(v)
  }
  invisible(dev.off())
}

main <- function() {
  # args <- c('testplot.png', 'testnames.txt', 'testlists.txt')
  args <- commandArgs(trailingOnly = TRUE)
  plotPath  <- args[[1]]
  namesPath <- args[[2]] # might contain "<<emptystr>>"?
  listsPath <- args[[3]] # might contain "<<emptystr>>"?
  read_named_lol(namesPath, listsPath) %>% plot_lists(plotPath)
}

main()
