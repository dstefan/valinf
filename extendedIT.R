
N = 10000

# initialize required global parameters
gol = c("cost", "market.share", "market.value")
dec = c("choose.one")
alt = c("alt1", "alt2")

# only one decision...
altFilter = c(0) # needed when more than one decision to filter chosen alternatives

decFx = c() # not needed because only one decision
benFx = "cost + market.share * market.value"

### Populate data,TODO: from csv file
sim = array(dim = c(2, 3, N))

# Cost, market.share abd market.value for alternative 1.
sim[1,1,] = rtriangle(N, 1, 4, 3)
sim[1,2,] = rtriangle(N, 0, 0.5, 0.1)
sim[1,3,] = rtriangle(N, 20, 60, 30)

# Cost and value for alternative 2.
sim[2,1,] = rtriangle(N, 1, 6, 5)
sim[2,2,] = rtriangle(N, 0, 0.5, 0.2)
sim[2,3,] = rtriangle(N, 20, 60, 30)
###

# Enlist all solutions as vectors
X = list(c(1), c(2))
# Compute the net benefit for all solutions
NB = NBAll(X)
evpi.all = evpiAll(NB, X)