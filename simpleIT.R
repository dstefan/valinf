
N = 10000

# initialize required global parameters
gol = c("cost", "value")
dec = c("choose.one")
alt = c("alt1", "alt2")

# only one decision...
altFilter = c(0) # needed when more than one decision to filter chosen alternatives

decFx = c() # not needed because only one decision
benFx = "cost + value"

### Populate data,TODO: from csv file
sim = array(dim = c(2, 2, N))

# Cost and value for alternative 1.
sim[1,1,] = rtriangle(N, 1, 4, 3)
sim[1,2,] = rtriangle(N, 2, 6, 4)

# Cost and value for alternative 2.
sim[2,1,] = rtriangle(N, 2.9, 3.1, 3)
sim[2,2,] = rtriangle(N, 3.5, 4.5, 4)
###

# Enlist all solutions as vectors
X = list(c(1), c(2))
# Compute the net benefit for all solutions
NB = NBAll(X)
evpi.all = evpiAll(NB, X)