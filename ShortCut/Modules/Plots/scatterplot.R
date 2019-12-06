#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(readr))

# TODO read scores and plot two variables here

read_nums <- function(numsPath)
  read.table(numsPath, col.names=c('score', 'value')) %>% tbl_df

plot_nums <- function(nums, titlePath, xlabPath) {
  p <- ggplot(data=nums, aes(x=value, y=score)) +
         geom_point(size=2) +
	 scale_x_log10()
  title <- read_string(titlePath)
  label <- read_string(xlabPath)
  if (title != "<<emptystr>>") { p <- p + ggtitle(title) }
  # TODO do this in read_nums?
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
  numsPath  <- args[[3]]
  xlabPath  <- args[[4]] # might contain "<<emptystr>>"
  read_nums(numsPath) %>%
    plot_nums(titlePath, xlabPath) %>%
    save_plot(plotPath)
}

main()
