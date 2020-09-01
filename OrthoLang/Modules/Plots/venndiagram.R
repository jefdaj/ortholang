#!/usr/bin/env Rscript

# TODO should these have titles too? ask people
# TODO what if no varnames? i guess name by the cachedlines basename? handle that in haskell?

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(readr)) # TODO remove?
suppressPackageStartupMessages(require(gridExtra)) # TODO remove?

# turn off the .log files
# from https://stackoverflow.com/a/36812214
suppressPackageStartupMessages(require(futile.logger))
invisible(futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger"))

# used in the read functions below
fix_ol_path <- function(filename)
  gsub('\\$TMPDIR' , Sys.getenv('TMPDIR') ,
  gsub('\\$WORKDIR', Sys.getenv('WORKDIR'), filename))

read_list <- function(filename)
  # read one list
  filename %>% fix_ol_path %>% scan(what=character(), quiet=TRUE)

#TODO can this be removed now?
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

shorten_labels <- function(labels)
  # label includes the type, which we cut off because it will always be 'num.list'
  # strsplit(labels, split='\\.')[[1]][1]
  sapply(strsplit(labels, split='\\.'), function(l) l[1])

read_lol <- function(filename) {
  # read a list of lists
  filenames <- read_list(filename) %>% lapply(fix_ol_path)
  lapply(setNames(filenames, sub('.txt', '', filenames)), read_list)
}

read_named_lol <- function(namesfile, listsfile) {
  # listnames <- read_list(namesfile)
	# cat(paste0('listnames:', listnames, '\n'))
  listnames <- read_list(namesfile) %>% shorten_labels # %>% fix_unnamed
	# cat(paste0('listnames:', listnames, '\n'))
  lists <- read_lol(listsfile)
	# cat(paste0('lists:', lists, '\n'))
  stopifnot(length(listnames) == length(lists))
  names(lists) <- listnames
  return(lists)
}

read_string <- function(textFile)
  read_file(textFile) %>% trimws

plot_lists <- function(lists, titlePath, outPath) {
  # The best way I can think to do this with no interaction is to draw the kind
  # of Venn diagrams people expect with up to 5 sets, and switch to upset for more.

  title <- read_string(titlePath)
  if (title == "<<emptystr>>") { title <- "" }
 
  nlists <- length(lists)
  png(filename=outPath, width=600, height=600)
  if (nlists > 5) {
    suppressPackageStartupMessages(require(UpSetR))
    p <- upset(fromList(lists), title=title)
    print(p) # for some reason, "print"ing it is necessary
		# TODO does this work? https://github.com/hms-dbmi/UpSetR/issues/76
		# grid.text(,x = 0.65, y=0.95, gp=gpar(fontsize=20))
  } else {
    suppressPackageStartupMessages(require(gridExtra))
    suppressPackageStartupMessages(require(VennDiagram))
    # adding 2 makes it a nice green/blue/purple theme
    v <- venn.diagram(lists, fill = 2+(1:nlists), alpha=0.5, filename=NULL)
    # viewport prevents labels getting cut off
    # from https://stackoverflow.com/a/22826211
    # grid.newpage()
    # pushViewport(viewport(width=unit(0.95, "npc"), height = unit(1, "npc")))
    # grid.draw(v)
		grid.arrange(gTree(children=v), top=title)
  }
  invisible(dev.off())
}

main <- function() {
  # args <- c('testplot.png', 'testnames.txt', 'testlists.txt')
  args <- commandArgs(trailingOnly = TRUE)
  plotPath  <- args[[1]]
  titlePath <- args[[2]] # might contain "<<emptystr>>"?
  namesPath <- args[[3]] # might contain "<<emptystr>>"?
  listsPath <- args[[4]] # might contain "<<emptystr>>"?
  read_named_lol(namesPath, listsPath) %>% plot_lists(titlePath, plotPath)
}

main()
