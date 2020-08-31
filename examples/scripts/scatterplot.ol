# for now, need to do a repeat to get scores
xvar = 1            # labels the x axis
yvar = 1.2e3 * xvar # labels the y axis (named plot only)
named = score_repeats yvar xvar [1,2,3,4,5,2,2,4,1,3,1,1,1,1]
plots =
  [ scatterplot "yvars for a list of xvar values" named
  , scatterplot "same but with unnamed (inline) yvar"
                (score_repeats (1.2e3 * xvar) xvar [1,2,3,4,5,2,2,4,1,3,1,1,1,1])
  ]
result = plots
