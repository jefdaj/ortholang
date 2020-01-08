#!/usr/bin/env Rscript

# This one is pretty messy. Is there a much simpler way to express it?

range_multiply <- function(start, stop, factor) {
  if (start == stop) { 
		# singleton, so factor doesn't matter
    return(start)
  } else {
		stopifnot(start != 0 & stop != 0 & factor != 0)
		stopifnot((start < 0) == (stop < 0))
  	if (abs(start) < abs(stop)) {
  		# range should be increasing
  		stopifnot(factor > 1)
      current <- start
      accumulated <- start
      while (abs(factor * current) <= abs(stop)) {
  			# cat(paste('current: ', current, 'factor:', factor, 'accumulated:', accumulated, '\n'))
        current <- current * factor
  	    accumulated <- c(accumulated, current)
  	  }
  	  return(accumulated)
    } else {
  		# start > stop, range should be decreasing
  		stopifnot(0 < factor & factor < 1)
      current <- start
      accumulated <- start
      while (abs(factor * current) >= abs(stop)) {
  			# cat(paste('current: ', current, 'factor:', factor, 'accumulated:', accumulated, '\n'))
        current <- current * factor
  	    accumulated <- c(accumulated, current)
  	  }
  	  return(accumulated)
    }
	}
}

save_list <- function(outPath, elems) {
	con <- file(outPath)
  if (length(elems) == 0) {
      elems <- "<<emptylist>>"
  }
  writeLines(as.character(elems), con)
	close(con)
}

main <- function() {
  args   <- commandArgs(trailingOnly = TRUE)
  path   <- args[[1]]
  start  <- as.numeric(args[[2]])
  stop   <- as.numeric(args[[3]])
  factor <- as.numeric(args[[4]])
	nums   <- range_multiply(start, stop, factor)
  save_list(path, nums)
}

main()
