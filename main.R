
library(triangle)

source("guidearch.model.R")
source("utils.R")
source("wmra-0.02.R")

N = 1000 # number of simulations
must = -1000 # a random must value beasd on my knowledge of the resulting profit range
S = gen.solutions(c(2, 2, 2, 2, 2, 3, 2, 4, 3, 3)) # list of solutions
Om = simulate.data("guidearch.data.csv", N) # parameters

profit = rep(NA, length(S))
risk = rep(NA, length(S))
  
cat("\n", "Compute profit/risk for ", length(S), " solutions.", "\n\n", sep="")

# Compute profit and risk for all solutions.
for (i in 1:length(S)) {
  s = S[[i]] # get next solution vector
  P = Profit(s, Om) # compute profit for given solution and known params
  profit[i] = mean(P)
  risk[i] = (sum(P < must) / length(P)) * 100
  if (i %% 100 == 0) {
    cat(round((i / length(S)) * 100), "%", "\n")
  }
}

# Bind profit and risk columnts into one matrix.
PR = cbind(profit, risk)

# Plot solutions where risk is less than 10%.
plot(PR[PR[,"risk"] < 10,])

# Select solution where risk is less than 10%.
Spr = S[which(PR[,"risk"] < 10)]

# TODO: Analysis of risks, evpi, etc. for the pruned solution space.

