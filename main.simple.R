
library("triangle")

source("wmra-0.04.R")
source("model.simple.R")

N = 10000
must = 0

params = load.params("simple.data.labels.csv")
rparams = realize.params(N, params)
assign.options()

# Solutions matrix (vector in this case)
S = c(1, 2)

# Compute cost, benefit and NB for both solutions
s1.cost = Cost(c(1), rparams)
s1.benefit = Benefit(c(1), rparams)

s2.cost = Cost(c(2), rparams)
s2.benefit = Benefit(c(2), rparams)

s1.NB = s1.benefit - s1.cost
s2.NB = s2.benefit - s2.cost

NB.all = cbind(s1.NB, s2.NB)

# Compute the overall EVPI
EVPI = evpi(NB.all)

s1.cost.BRPPI = brppi(s1.cost, NB.all, must)
s1.share.BRPPI = brppi(Market.share(S[1], rparams), NB.all, must)
s1.benefit.BRPPI = brppi(s1.benefit, NB.all, must)

s2.cost.BRPPI = brppi(s2.cost, NB.all, must)
s2.share.BRPPI = brppi(Market.share(S[2], rparams), NB.all, must)
s2.benefit.BRPPI = brppi(s2.benefit, NB.all, must)

market.size.BRPPI = brppi(Market.size(S[1], rparams), NB.all, must)

res = list()

no.info = array(dim=c(2, 3))
cost.info = array(dim=c(2, 3))
share.info = array(dim=c(2, 3))
benefit.info = array(dim=c(2, 3))

dimnames(no.info) = list(c('System 1', 'System 2'), c('ENB', 'risk', 'PLM'))
dimnames(cost.info) = list(c('of System 1', 'of System 2'), c('ENB', 'risk', 'PLM'))
dimnames(share.info) = list(c('of System 1', 'of System 2'), c('ENB', 'risk', 'PLM'))
dimnames(benefit.info) = list(c('of System 1', 'of System 2'), c('ENB', 'risk', 'PLM'))

no.info[,1] = round(enb(NB.all))
no.info[,2] = round(risk(NB.all, 0), 2)
no.info[,3] = round(plm(NB.all, 0))

cost.info[1,1] = round(s1.cost.BRPPI$NB)
cost.info[1,2] = round(s1.cost.BRPPI$risk, 2)
cost.info[1,3] = round(s1.cost.BRPPI$plm)

cost.info[2,1] = round(s2.cost.BRPPI$NB)
cost.info[2,2] = round(s2.cost.BRPPI$risk, 2)
cost.info[2,3] = round(s2.cost.BRPPI$plm)

share.info[1,1] = round(s1.share.BRPPI$NB)
share.info[1,2] = round(s1.share.BRPPI$risk, 2)
share.info[1,3] = round(s1.share.BRPPI$plm)

share.info[2,1] = round(s2.share.BRPPI$NB)
share.info[2,2] = round(s2.share.BRPPI$risk, 2)
share.info[2,3] = round(s2.share.BRPPI$plm)

benefit.info[1,1] = round(s1.benefit.BRPPI$NB)
benefit.info[1,2] = round(s1.benefit.BRPPI$risk, 2)
benefit.info[1,3] = round(s1.benefit.BRPPI$plm)

benefit.info[2,1] = round(s2.benefit.BRPPI$NB)
benefit.info[2,2] = round(s2.benefit.BRPPI$risk, 2)
benefit.info[2,3] = round(s2.benefit.BRPPI$plm)

res$no.info = no.info
res$best.option = best(NB.all)$id
res$best.ENB = best(NB.all)$NB

res$market.size = market.size.BRPPI
res$cost.info = cost.info
res$share.info = share.info
res$benefit.info = benefit.info
res$evpi = evpi(NB.all)

print(res)