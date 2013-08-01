
gen.solutions = function(dnum) {
  #
  # Generates all solution vectors given vector that contains numbers of
  # alternatives for each decision.
  #
  # Args:
  #   dnum: Vector with numnbers of alternatvies for each decision.
  #         E.g. vector c(2, 3) would correspond to a model with two
  #         decisions with two and three alternatives respectively.
  #
  # Returns:
  #   A list with all solution vectors.
  #
  
  n = prod(dnum)
  l = length(dnum)
  s = rep(1,l)
  S = list()
  
  c = 1
  
  for (i in 1:n) {
    S[[i]] = s
    while (c <= l && s[c] + 1 > dnum[c]) {
      s[c] = 1
      c = c + 1
    }
    if (c > l) {
      break
    }
    s[c] = s[c] + 1
    c = 1
  }
  return(S)
}

simulate.data = function(fname, N) {
  #
  # Reads elicited data from a .csv file and simulates using triangular
  # distribution.
  #
  # Args:
  #   fname: File name of the .csv file.
  #   N: Number of iteration for the simulation.
  #
  # Returns:
  #   A 3D array with columns corresponding to goals, rows to alternatives where
  #   each cell contains N simulated values for corresponding goal x alternative.
  #
  
  Om = array(dim = c(25, length(G), N))
  dimnames(Om) = list(c(), names(G), c())
  data = read.csv(fname, header=FALSE)
  
  for (i in 1:nrow(data)) {
    for (j in 1:(ncol(data)/3)) {
      a = data[i,(j-1)*3+1]
      b = data[i,(j-1)*3+3]
      c = data[i,(j-1)*3+2]
      if (a > c) {
        temp = a;
        a = b;
        b = temp;
      }
      Om[i,j,] = rtriangle(N, a, b, c)
    }
  }
  
  return(Om)
}