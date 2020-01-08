#!/usr/bin/env Rscript

# TODO need a separate list of names that will come from haskell variables

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))

read_list <- function(filename)
  # read one list
  scan(filename, what=character(), quiet=TRUE)

read_lol <- function(filename) {
  # read a list of lists
  filenames <- read_list(filename) # TODO sort?
  lapply(setNames(filenames, sub('.txt', '', filenames)), read_list) # TODO sub other extensions too?
}

read_named_lol <- function(namesfile, listsfile) {
  listnames <- read_list(namesfile)
  lists <- read_lol(listsfile)
  stopifnot(length(listnames) == length(lists))
  names(lists) <- listnames
  return(lists)
}

lists_to_table <- function(lol) {
  # make a "set membership table" with rows for set elements and cols for sets
  members <- unlist(lol) %>% unique
  m <- sapply(names(lol), function(n) sapply(members, function(m) ifelse(m %in% lol[[n]], 'x', '')))
  df <- cbind(members, cbind(m))
  colnames(df) <- c('', names(lol))
  df <- df[ order(as.numeric(row.names(df))),]
  return(df)
}

write_table <- function(df, filename)
  write.table(df, filename, sep='\t', quote=FALSE, row.names=FALSE)

main <- function() {
  # read_lol('files.txt') %>% lists_to_table %>% write_table('test.tsv')
  args <- commandArgs(trailingOnly = TRUE)
  tsvPath   <- args[[1]]
  namesPath <- args[[2]] # might contain "<<emptystr>>"?
  listsPath <- args[[3]] # might contain "<<emptystr>>"?
  read_named_lol(namesPath, listsPath) %>% lists_to_table %>% write_table(tsvPath)
}

main()
