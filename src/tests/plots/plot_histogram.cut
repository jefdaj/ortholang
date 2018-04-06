# histograms don't require a repeat; can use any num.list
xvar = 1            # labels the x axis
yvar = 1.2e3 * xvar # labels the y axis (named plot only)
scores = score_repeats yvar xvar [1,2,3,4,5,2,2,4,1,3,1,1,1,1]
xvars = extract_scored scores
yvars = extract_scores scores
plots =
  [ histogram "histogram of xvar inputs" xvars
  , histogram "histogram of yvar scores" yvars
  , histogram "histogram of unnamed (inline) nums" [1,2,3,4,5,2,2,4,1,3,1,1,1,1]
  ]
result = plots
