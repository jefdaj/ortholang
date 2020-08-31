#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(readr))

# TODO read scores and plot two variables here

read_scores <- function(numsPath)
  read.table(numsPath, col.names=c('score', 'value')) %>% tbl_df

plot_nums <- function(nums, titlePath, xlabPath) {
  p <- ggplot(data=nums, aes(x=value, y=score)) +
         geom_point(size=2) +
   scale_x_log10()
  title <- read_string(titlePath)

  # label includes the type, which we cut off because it will always be 'num.list'
  label <- strsplit(read_string(xlabPath), split='\\.')[[1]][1]

  if (title != "<<emptystr>>") { p <- p + ggtitle(title) }
  # TODO do this in read_scores?
  if (label != "<<emptystr>>") {
    p <- p + xlab(label)
  } else {
    p <- p + xlab("") # remove 'value'
  }
  return(p)
}

read_string <- function(textFile)
  read_file(textFile) %>% trimws

save_plot <- function(plot, plotPath)
  suppressMessages(ggsave(plot, filename=plotPath, device="png"))

main <- function() {
  # args <- c('testplot.png', 'testtitle.txt', 'testnums.txt', 'testxlab.txt')
  args <- commandArgs(trailingOnly = TRUE)
  plotPath  <- args[[1]]
  titlePath <- args[[2]] # might contain "<<emptystr>>"
  xlabPath  <- args[[3]] # might contain "<<emptystr>>"
  numsPath  <- args[[4]]
  read_scores(numsPath) %>%
    plot_nums(titlePath, xlabPath) %>%
    save_plot(plotPath)
}

main()
