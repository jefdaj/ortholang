#!/usr/bin/env Rscript

# TODO need a separate list of names that will come from haskell variables

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))

# used in the read functions below
fix_ol_path <- function(filename)
  gsub('\\$TMPDIR' , Sys.getenv('TMPDIR') ,
  gsub('\\$WORKDIR', Sys.getenv('WORKDIR'), filename))

read_list <- function(filename)
  # read one list
  filename %>% fix_ol_path %>% scan(what=character(), quiet=TRUE)

shorten_labels <- function(labels)
  # label includes the type, which we cut off
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
