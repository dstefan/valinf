
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
I = rep(1:nrow(S))

val = array(dim=c(nrow(S), 3))
val.max = array(dim=c(nrow(S), 3))

fail = rep(NA, nrow(S))

for (i in 1:nrow(S)) {
  
  print.status(i, nrow(S))
  
  cost = mean(Cost(S[i,], rparams))
  utility = mean(Efficiency(S[i,], rparams))
  risk = Failure.risk(S[i,], rparams)
    
  val[i,] = c(cost, utility, risk)
  val.max[i,] = c(-cost, utility, -risk)
}
cat("\n")

# Pareto-strip error
error = c(800, 0.5)

res = pareto(I, val.max[,c(1,2)])
res$strip = pareto.strip(res$pfront, res$domin, val.max[,c(1, 2)], error)

plot(val[,c(1,2)], xlab="Cost", ylab="Efficiency", pch=4, col=8)
points(val[res$strip,c(1,2)], col=3, pch=1)
points(val[res$pfront,c(1,2)], col=2, pch=1)

# Willingness to pay
k = 200000
must = 1000000
SShort = S[res$pfront,]
NB = array(dim=c(1000, nrow(SShort)))
info = array(dim=c(nrow(SShort) * 3, 5))

for (i in 1:nrow(SShort)) {
  NB[,i] = Benefit(k, SShort[i,], rparams)
}

ENB = colMeans(NB)
ENB.best = ENB[which.max(ENB)]

for (i in 1:nrow(SShort)) {
  BRPPI = brppi(Cost(SShort[i,], rparams), NB, must)
  info[(i-1)*3+1,1] = BRPPI$NB - ENB.best
  info[(i-1)*3+2,1] = BRPPI$risk
  info[(i-1)*3+3,1] = BRPPI$plm
}

for (i in 1:nrow(SShort)) {
  BRPPI = brppi(Efficiency(SShort[i,], rparams), NB, must)
  info[(i-1)*3+1,2] = BRPPI$NB - ENB.best
  info[(i-1)*3+2,2] = BRPPI$risk
  info[(i-1)*3+3,2] = BRPPI$plm
}

for (i in 1:nrow(SShort)) {
  BRPPI = brppi(Battery.life(SShort[i,], rparams), NB, must)
  info[(i-1)*3+1,3] = BRPPI$NB - ENB.best
  info[(i-1)*3+2,3] = BRPPI$risk
  info[(i-1)*3+3,3] = BRPPI$plm
}

for (i in 1:nrow(SShort)) {
  BRPPI = brppi(Resp.time(SShort[i,], rparams), NB, must)
  info[(i-1)*3+1,4] = BRPPI$NB - ENB.best
  info[(i-1)*3+2,4] = BRPPI$risk
  info[(i-1)*3+3,4] = BRPPI$plm
}

for (i in 1:nrow(SShort)) {
  BRPPI = brppi(Reliability(SShort[i,], rparams), NB, must)
  info[(i-1)*3+1,5] = BRPPI$NB - ENB.best
  info[(i-1)*3+2,5] = BRPPI$risk
  info[(i-1)*3+3,5] = BRPPI$plm
}

#Cost
#Efficiecy
#Battery.life
#Resp.time
#Reliability