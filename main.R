
library("triangle")

source("wmra-0.04.R")
source("model.R")

N = 1000

params = load.params("guidearch.data.labels.csv")
rparams = realize.params(N, params)

# Create function from options.
assign.options()

# Generate all solution vectors.
S = gen.solutions(c(2, 2, 2, 2, 2, 3, 2, 4, 3, 3))
# Create a vector with solution indices.
I = rep(1:length(S))

# Container for "utility vectors" for all solutions.
val = array(dim=c(2,length(S)))
val.min = array(dim=c(2,length(S)))

for (i in 1:length(S)) {
  
  print.status(i, length(S))
  
  cost = mean(Cost(S[[i]], rparams))
  utility = mean(Utility(S[[i]], rparams))

  val[,i] = c(cost, utility)
  val.min[,i] = c(cost, -utility)
}

# Pareto-strip error
error = c(60, 1.6)

res = pareto(I, val.min)
res$strip = pareto.strip(res$pfront, res$domin, val.min, error)

plot(t(val), xlab="Cost", ylab="Utility", pch=4, col=8)
points(t(val[,res$pfront]), col=2, pch=1)
points(t(val[,res$strip]), col=3, pch=1)
