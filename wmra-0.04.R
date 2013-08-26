
assign.options = function() {
  #
  # Creates a global function for each option in the options vector that takes
  # an output name and realized parameters as argument and returns option's
  # impact on the given output.
  #
  
  for (i in 1:length(decisions)) {
    assign(decisions[i], (
      function() {
        dnum = i
        function(oname, s, rparam) {
          row = dMap[dnum] + s[dnum]
          return(rparam[row, oname, ])
        }
      }
    ) (), envir=.GlobalEnv)
  }
}

load.params = function(fname) {
  #
  # Loads parameters from a .csv file. While at it, also makes global vectos
  # with outcomes, decisions, options and dMap.
  #
  # TODO: Describe .csv format!
  #
  # Args:
  #   fname: Name of the .csv file.
  #
  # Returns:
  #   A matrix with loaded parameters.
  #
  
  # Load file with data
  csv = read.csv(fname, header=FALSE)
  
  # Extract outcomes.
  oTemp = as.vector(csv[1,3:ncol(csv)])
  outcomes = oTemp[!is.na(oTemp)]
  
  # Extract decisions
  dTemp = as.vector(csv[2:nrow(csv),1])
  dPos = which((dTemp != "") != FALSE)
  dPat = rep(seq_along(dPos), times=diff(c(dPos, length(dTemp) + 1)))
  decisions = dTemp[dTemp != ""]
  
  # Extract options
  oTemp = as.vector(csv[2:nrow(csv),2])
  options = split(oTemp, dPat)
  names(options) = decisions
  
  # Convert params into a data matrix
  params = matrix(as.numeric(as.matrix(csv[2:nrow(csv),3:ncol(csv)])), byrow=FALSE, ncol=length(outcomes)*3)
  
  # Assign all variables to the given environment.
  assign("outcomes", outcomes, envir=.GlobalEnv)
  assign("decisions", decisions, envir=.GlobalEnv)
  assign("dMap", dPos - 1, envir=.GlobalEnv)
  assign("options", options, envir=.GlobalEnv)
  
  return(params)
}

realize.params = function(N, params) {
  #
  # Simulates parameters with N runs of MonteCarlo simulation and returns
  # resulting values as a 3D matrix.
  #
  # TODO: Enable overriding the type of distribution used in the simulation.
  #
  # Args:
  #   N: Desired number of MC runs.
  #   params: Parameters to be simulated.
  #
  # Returns:
  #   A 3D matrix with realized parameters.
  #
  
  # Allocate space for results.
  rparams = array(dim = c(length(unlist(options)), length(outcomes), N))
  # Name matrix x,y dimensions with outcomes and decision names.
  dimnames(rparams) = list(unlist(options, use.names=FALSE), outcomes, c())
  
  for (i in 1:nrow(params)) {
    for (j in 1:(ncol(params)/3)) {
      a = params[i,(j-1)*3+1]
      b = params[i,(j-1)*3+3]
      c = params[i,(j-1)*3+2]
      if (a > b) {
        temp = a;
        a = b;
        b = temp;
      }
      rparams[i,j,] = rtriangle(N, a, b, c)
    }
  }
  
  return(rparams)
}

gen.solutions = function(dnum) {
  #
  # Generates all solution vectors given a 'constrain' vector that contains
  # numbers of alternatives for each decision.
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
  S = array(dim=c(n, l))
  
  c = 1
  
  for (i in 1:n) {
    S[i,] = s
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

pareto = function(I, V) {
  
  cat("Computing pareto front, please wait...\n")
  
  domin = c()
  i = 1
  
  while (i < length(I)) {
    j = i + 1
    while (j <= length(I)) {
      if (all(V[I[i],] < V[I[j],])) {
        domin = c(domin, I[i])
        I = I[-i]
        j = i + 1
        next
      }
      if (all(V[I[i],] > V[I[j],])) {
        domin = c(domin, I[j])
        I = I[-j]
        next
      }
      j = j + 1
    }
    i = i + 1
  }
  
  return(list(pfront=I, domin=domin))
}

pareto.strip = function(pfront, domin, V, err) {
  
  cat("Computing pareto strip, please wait...\n")
  
  nonDom = TRUE
  str = c()
  
  for (i in 1:length(domin)) {
    impr = V[domin[i],] + err
    for (j in 1:length(pfront)) {
      if (all(impr < V[pfront[j],])) {
        nonDom = FALSE
        break
      }
    }
    if (nonDom) {
      str = c(str, domin[i])
      
    }
    nonDom = TRUE
  }
  
  return(str)
}

rowMax = function(m) {
  apply(m, 1, max)
}

rowMin = function(m) {
  apply(m, 1, min)
}

colMax = function(m) {
  apply(m, 2, max)
}

colMin = function(m) {
  apply(m, 2, min)
}

print.status = function(pos, end) {
  if (pos %% 100 == 0) {
    cat(round(pos/end, 2)*100, "%", "\n")
  }
  else {
    cat(".")
  }
}

best = function(NB) {
  
  ENB = colMeans(NB)
  id = which.max(ENB)
  
  names(ENB) = NULL
  names(id) = NULL
  
  return(list(id=id, NB=ENB[id]))
}

enb = function(NB) {
  ENB = colMeans(NB)
  names(ENB) = rep(1:ncol(NB))
  return(ENB)
}

risk = function(NB, must) {
  is.loss = NB < must
  risk = colMeans(is.loss)
  names(risk) = rep(1:ncol(NB))
  return(risk)
}

safest.id = function(NB, must) {
  risk = risk(NB, must)
  id = which.min(risk)
  names(id) = NULL
  return(id)
}

plm = function(NB, must) {
  #
  # Computes a vector with probable loss magnitude (PLM) given the net benefits
  # and must values.
  #
  # Author: DS
  #
  # Args:
  #   NB: Matrix with net benefits in columns.
  #   must: Must value for net benefit not to result in a loss.
  #
  # Returns:
  #   The probable loss magnitude vector.
  #
  
  # Where does loss occur?
  is.loss = NB < must
  
  # How big is the loss (when it occurs)?
  loss = (NB - must) * is.loss
  
  # Divide the sum of losses by the number of loss occurances to get plm.
  plm = colSums(loss) / colSums(is.loss)
  
  # Remove possible division by zero (-Inf) results (no loss occured).
  plm[is.infinite(plm)] = 0
  
  # Cosmetic adjustment - name columns by indices
  names(plm) = rep(1:ncol(NB))
  
  return (-1*plm)
}

evpi = function(NB) {
  
  id = best(NB)$id
  
  ENB.no.info = colMeans(NB)
  ENB.all.info = mean(rowMax(NB))
  evpi = ENB.all.info - ENB.no.info[id]
  names(evpi) = NULL
  
  return(evpi)
}

evppi = function(param, nbs, ...){
  # Author: EL
  # if parameter is not stochastic, return 0 else compute evpi
  if(length(unique(param))==1) {return(0)}
  evppi.func(param, nbs, ...)$evppi
}

evppi.func <- function(param,nbs,...) {
  evppi.graphics <- FALSE
  exArgs <- list(...)
  if(length(exArgs)>0)
    if(exists("evppi.graphics",where=exArgs))
      evppi.graphics=exArgs$evppi.graphics
  
  ## Computes the Expected Value of Partial Information with respect to a given parameter
  ## using the algorithm for fast computation of Sadatsafavi et al (2013)
  ## source: http://webservices.core.ubc.ca/research/other/1evppi/
  # param = the parameter of interest (must be in the format of a n.sim x 1 simulations from its posterior
  # nbs = a matrix of net benefits for the interventions being compared 
  if(length(unique(param))==1)
    stop("Error: input parameter is not stochastic")
  n<-length(param)
  o<-order(param)
  param<-param[o]
  d<-dim(nbs)[2]
  if(is.vector(nbs))
  {
    nbs<-cbind(nbs[o],0)
    d<-2
  }
  if(d==1)
  {
    nbs<-cbind(nbs[o,],0)
    d<-2
  }
  else
    nbs<-nbs[o,]
  nSegs<-matrix(1,d,d)
  exArgs<-list(...)
  for(obj in exArgs)
  {
    if(is.null(names(obj)))
      if (length(obj)==1)
      {nSegs[1,2]<-obj; nSegs[2,1]<-obj}
    else
    {nSegs[obj[1],obj[2]]<-obj[3]; nSegs[obj[2],obj[1]]<-obj[3];}
  }
  segPoints<-c()
  for(i in 1:(d-1))
    for(j in (i+1):d)
    {
      ###    	message(paste('Fitting ',nSegs[i,j],' segmentation points for decisions ',i,j))
      cm<-cumsum(nbs[,i]-nbs[,j])/n
      if(nSegs[i,j]==1)
      {
        l<-which.min(cm)
        u<-which.max(cm)
        if(cm[u]-max(cm[1],cm[n])>min(cm[1],cm[n])-cm[l])
          segPoint<-u
        else
          segPoint<-l
        if (segPoint>1 && segPoint<n)
          segPoints<-c(segPoints, segPoint)
      }
      if(nSegs[i,j]==2)
      {
        distMaxMin<-0
        distMinMax<-0
        minL<-Inf
        maxL<--Inf
        for(k in 1:n)
        {
          #max-min pattern
          if(cm[k]>maxL)
          {
            maxLP<-k
            maxL<-cm[k]
          }
          else
            if(maxL-cm[k]>distMaxMin)
            {
              distMaxMin<-maxL-cm[k]
              segMaxMinL<-maxLP	
              segMaxMinR<-k
            }	
          #min-max pattern
          if(cm[k]<minL)
          {
            minLP<-k
            minL<-cm[k]
          }
          else
            if(cm[k]-minL>distMinMax)
            {
              distMinMax<-cm[k]-minL
              segMinMaxL<-minLP	
              segMinMaxR<-k
            }	
        }
        siMaxMin<-cm[segMaxMinL]+distMaxMin+(cm[n]-cm[segMaxMinR])
        siMinMax<--cm[segMaxMinL]+distMinMax-(cm[n]-cm[segMinMaxR])
        if(siMaxMin>siMinMax)
        {
          segPoint<-c(segMaxMinL,segMaxMinR)
        }
        else
        {
          segPoint<-c(segMinMaxL,segMinMaxR)
        }
        if (segPoint[1]>1 && segPoint[1]<n)
          segPoints<-c(segPoints, segPoint[1])
        if (segPoint[2]>1 && segPoint[2]<n)
          segPoints<-c(segPoints, segPoint[2])
      }
      if(exists('evppi.graphics') && evppi.graphics==TRUE){
        dev.new()
        plot(param,cm)
        for(k in 1:length(segPoint))
        {
          if (segPoint[k]>1 && segPoint[k]<n)
            lines(c(param[segPoint[k]],param[segPoint[k]]),c(min(cm),max(cm)))
        }
        title(paste("Decision",i,"vs.",j))
      }
      else
      {
        evppi.graphics = FALSE
        ###  			message("Graphic output is disabled. Set evppi.graphics=TRUE to enable graphics")
      }
    }
  if(length(segPoints)>0)
  {
    segPoints2<-unique(c(0, segPoints[order(segPoints)], n))
    evppi<-0
    for(j in 1:(length(segPoints2)-1))
      evppi<-evppi+max(colSums(matrix(nbs[(1+segPoints2[j]):segPoints2[j+1],],ncol=d)))/n    
    evppi<-evppi-max(colMeans(nbs))
    return(list(evppi=evppi,segPoints=param[segPoints2[-c(1,length(segPoints2))]]))
  }
  else
    evppi<-0
  return(list(evppi=evppi,segPoints=c()))     
}

###### brpi - benefit and risk with perfect information ##########################
brppi <- function(param, nbs, must,...) {
  evppi.graphics <- FALSE
  exArgs <- list(...)
  if(length(exArgs)>0)
    if(exists("evppi.graphics",where=exArgs))
      evppi.graphics=exArgs$evppi.graphics
  
  # Computes the expected benefits and risk assuming perfect information about
  # the parameter of interest. The method uses the segmentation method for 
  # computing evppi from Sadatsafavi et al (2013)
  # source: http://webservices.core.ubc.ca/research/other/1evppi/
  
  # Inputs
  # param = the parameter of interest (must be in the format of a n.sim x 1 simulations from its posterior
  # nbs = a matrix of net benefits for the interventions being compared 
  # must = a minumin net benefit value to be achieved. risk is defined as P(nbs<must)
  
  #Outputs
  # $ENB the expected net benefit with perfect information
  # $failiure.risk the risk of failure with perfect information
  # $expected.loss the expected loss with perfect information
  # $Segpoints: the segmentation points used for computing evpi
  
  #if input param is not stochastic, then no gain from perfect info
  if(length(unique(param))==1) {return(list(NB=NA, failure.risk=NA, expected.loss=NA, segPoints=NA))} 
  # stop("Error: input parameter is not stochastic")
  n<-length(param)
  o<-order(param)
  param<-param[o]          #sorts param in ascending order
  d<-dim(nbs)[2]
  if(is.vector(nbs))
  {
    nbs<-cbind(nbs[o],0)
    d<-2
  }
  if(d==1)
  {
    nbs<-cbind(nbs[o,],0)
    d<-2
  }
  else
    nbs<-nbs[o,]
  
  # finding the segementation points
  nSegs<-matrix(1,d,d)
  exArgs<-list(...)
  for(obj in exArgs)
  {
    if(is.null(names(obj)))
      if (length(obj)==1)
      {nSegs[1,2]<-obj; nSegs[2,1]<-obj}
    else
    {nSegs[obj[1],obj[2]]<-obj[3]; nSegs[obj[2],obj[1]]<-obj[3];}
  }
  segPoints<-c()
  for(i in 1:(d-1))
    for(j in (i+1):d)
    {
      ###      message(paste('Fitting ',nSegs[i,j],' segmentation points for decisions ',i,j))
      cm<-cumsum(nbs[,i]-nbs[,j])/n
      if(nSegs[i,j]==1)
      {
        l<-which.min(cm)
        u<-which.max(cm)
        if(cm[u]-max(cm[1],cm[n])>min(cm[1],cm[n])-cm[l])
          segPoint<-u
        else
          segPoint<-l
        if (segPoint>1 && segPoint<n)
          segPoints<-c(segPoints, segPoint)
      }
      if(nSegs[i,j]==2)
      {
        distMaxMin<-0
        distMinMax<-0
        minL<-Inf
        maxL<--Inf
        for(k in 1:n)
        {
          #max-min pattern
          if(cm[k]>maxL)
          {
            maxLP<-k
            maxL<-cm[k]
          }
          else
            if(maxL-cm[k]>distMaxMin)
            {
              distMaxMin<-maxL-cm[k]
              segMaxMinL<-maxLP	
              segMaxMinR<-k
            }	
          #min-max pattern
          if(cm[k]<minL)
          {
            minLP<-k
            minL<-cm[k]
          }
          else
            if(cm[k]-minL>distMinMax)
            {
              distMinMax<-cm[k]-minL
              segMinMaxL<-minLP	
              segMinMaxR<-k
            }	
        }
        siMaxMin<-cm[segMaxMinL]+distMaxMin+(cm[n]-cm[segMaxMinR])
        siMinMax<--cm[segMaxMinL]+distMinMax-(cm[n]-cm[segMinMaxR])
        if(siMaxMin>siMinMax)
        {
          segPoint<-c(segMaxMinL,segMaxMinR)
        }
        else
        {
          segPoint<-c(segMinMaxL,segMinMaxR)
        }
        if (segPoint[1]>1 && segPoint[1]<n)
          segPoints<-c(segPoints, segPoint[1])
        if (segPoint[2]>1 && segPoint[2]<n)
          segPoints<-c(segPoints, segPoint[2])
      }
      if(exists('evppi.graphics') && evppi.graphics==TRUE){
        dev.new()
        plot(param,cm)
        for(k in 1:length(segPoint))
        {
          if (segPoint[k]>1 && segPoint[k]<n)
            lines(c(param[segPoint[k]],param[segPoint[k]]),c(min(cm),max(cm)))
        }
        title(paste("Decision",i,"vs.",j))
      }
      else
      {
        evppi.graphics = FALSE
        ###  			message("Graphic output is disabled. Set evppi.graphics=TRUE to enable graphics")
      }
    }
  # Post: SepPoints are defined
  
  # Computing expected net benefits and risks with perfect information
  if(length(segPoints)>0)
  {
    # splits the n simulations into segements at the segementation points
    segPoints2<-unique(c(0, segPoints[order(segPoints)], n))
    
    # fail and loss matrices, derived from nbs
    fail = nbs<must
    loss = (nbs-must) * fail
    
    #loop initialisation
    
    Sum.NB.with.perfect.info = 0
    Nbr.of.failure.with.perfect.info = 0
    Sum.of.loss.with.perfect.info = 0
    # Invariant: sums of NB, nbr failure, and losses over all simulations visited so far
    
    # loop iteration over each segment
    for(j in 1:(length(segPoints2)-1)){
      Sums.NB.in.segment = colSums(matrix(nbs[(1+segPoints2[j]):segPoints2[j+1],],ncol=d))
      best.in.segment = which.max(Sums.NB.in.segment)
      Sum.NB.for.best.in.segment = sum(nbs[(1+segPoints2[j]):segPoints2[j+1], best.in.segment])
      nbr.of.fail.for.best.in.segment = sum(fail[(1+segPoints2[j]):segPoints2[j+1], best.in.segment])
      Sum.of.loss.for.best.in.segment = sum(loss[(1+segPoints2[j]):segPoints2[j+1], best.in.segment])
      
      # updating the ouput variables
      Sum.NB.with.perfect.info = Sum.NB.with.perfect.info + Sum.NB.for.best.in.segment
      Nbr.of.failure.with.perfect.info = Nbr.of.failure.with.perfect.info + nbr.of.fail.for.best.in.segment
      Sum.of.loss.with.perfect.info = Sum.of.loss.with.perfect.info + Sum.of.loss.for.best.in.segment 
    }
    # generating the results
    return(list(
      NB = Sum.NB.with.perfect.info / n,
      risk = Nbr.of.failure.with.perfect.info / n,
      plm = -1 * Sum.of.loss.with.perfect.info / Nbr.of.failure.with.perfect.info,
      segPoints=param[segPoints2[-c(1,length(segPoints2))]]
    ))
  }
  else #no seg point -> no decision change, evpi = 0
    return(list(NB=NA, risk=NA, plm=NA, segPoints=NA))   
}