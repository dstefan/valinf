
dMap = rep(NA, length(D))
dc = 0

for (i in 1:length(D)) {
  dMap[i] = dc
  dc = dc + length(D[[i]])
}

eval = function(gname, s, Om) {
  #
  # Evaluate decisions' impact on the given goal.
  #
  # Args:
  #   gname: Goal name.
  #   s: Solution vector.
  #   Om: Known parameters.
  #
  
  environment(G[[gname]]) = environment()
  
  Om_s = Om[s + dMap, gname, ]
  for (i in 1:length(D)) {
    dname = names(D[i])
    assign(dname, Om_s[i,])
  }
  return(G[[gname]]())
}

g = function(gname, s, Om) {
  #
  # A shortcut to eval.
  #
  
  return(eval(gname, s, Om))
}

eval.all = function(s, Om) {
  #
  # Evaluate all goals from the decision model. Goal values will be made public
  # through global variables named by goal names from the model.
  #
  # Args:
  #   s: A solution vector.
  #   w: Known parameter values.
  #
  
  for (i in 1:length(G)) {
    gname = names(G[i])
    assign(gname, eval(gname, s, Om), envir=.GlobalEnv)
  }
}

