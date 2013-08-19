
library("triangle")

source("wmra-0.04.R")
source("model.R")

N = 1000

params = load.params("guidearch.data.labels.csv")
rparams = realize.params(N, params)

# Create function from options.
assign.options()

# Prepare extremes for each outuput for normalization in their respective functions.
dec.extremes = normalize.params(outputs, decisions, options, dMap, rparams)
dec.sums = lapply(dec.extremes, FUN=colSums)
MIN = dec.sums$min;
MAX = dec.sums$max;

# Generate all solution vectors.
S = gen.solutions(c(2, 2, 2, 2, 2, 3, 2, 4, 3, 3))

# Allocate space for results.
risk = rep(NA, length(S))
profit = rep(NA, length(S))

# Compute all profits and risks.
for (i in 1:length(S)) {
  if (i %% 100 == 0) {
    cat(round((i / length(S)) * 100), "%", "\n")
  }
  else {
    cat(".")
  }
  profitAll = Profit(S[[i]], rparams)
  profit[i] = mean(profitAll)
  risk[i] = 1 - mean(profitAll > 1.75)
}

res = cbind(risk, profit)

# Show solutions with risk lower than...
plot(res[res[,1]<0.4,])

# Extract solutions having risk lower than...
STrim = S[res[,1]<0.4]

# Allocate space for solution benefits
benefit = array(dim=c(N, length(STrim)))

for (i in 1:length(STrim)) {
  benefit[,i] = Benefit(STrim[[i]], rparams)
}



ENB = colMeans(benefit)
ENB.max = ENB[which.max(ENB)]

EVPI = list()
EVPI$overall = mean(rowMax(benefit)) - ENB.max

for (i in 1:length(STrim)) {
  EVPI[[paste("solution", i)]] = evpi(Benefit(STrim[[i]], rparams), benefit)
}

# Compute EVPI for specific outputs...
#evpi(Battery.usage(S[[1]], rparams), benefit)
#evpi(Reliability(S[[1]], rparams), benefit)

cat("\n\nResults:\n\n"); print(EVPI)