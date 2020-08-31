#!/usr/bin/env Rscript

# TODO pick binwidths or suppress the default message about it

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(readr))

read_nums <- function(numsPath)
  read.table(numsPath, as.is=TRUE) %>% tbl_df

plot_nums <- function(nums, titlePath, xlabPath) {
  p <- ggplot(data=nums, aes(x=V1)) + geom_histogram()
  title <- read_string(titlePath)

  # label includes the type, which we cut off because it will always be 'num.list'
  label <- strsplit(read_string(xlabPath), split='\\.')[[1]][1]

  if (title != "<<emptystr>>") { p <- p + ggtitle(title) }
  if (label != "<<emptystr>>") {
    p <- p + xlab(label)
  } else {
    p <- p + xlab("") # remove V1
  }
  return(p)
}

read_string <- function(textFile)
  read_file(textFile) %>% trimws

save_plot <- function(plot, plotPath)
  # TODO would png(); plot; dev.close() work around the X11 error here?
  suppressMessages(ggsave(plot, filename=plotPath, device="png"))

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  # args <- c('testplot.png', 'testtitle.txt', 'testnums.txt', 'testxlab.txt')
  plotPath  <- args[[1]]
  titlePath <- args[[2]] # might contain "<<emptystr>>"
  xlabPath  <- args[[3]] # might contain "<<emptystr>>"
  numsPath  <- args[[4]]
  read_nums(numsPath) %>%
    plot_nums(titlePath, xlabPath) %>%
    save_plot(plotPath)
}

main()
