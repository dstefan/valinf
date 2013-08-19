
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
  # with outputs, decisions, options and dMap.
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
  
  # Extract outputs.
  oTemp = as.vector(csv[1,3:ncol(csv)])
  outputs = oTemp[!is.na(oTemp)]
  
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
  params = matrix(as.numeric(as.matrix(csv[2:nrow(csv),3:ncol(csv)])), byrow=FALSE, ncol=21)
  
  # Assign all variables to the given environment.
  assign("outputs", outputs, envir=.GlobalEnv)
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
  rparams = array(dim = c(length(unlist(options)), length(outputs), N))
  # Name matrix x,y dimensions with outputs and decision names.
  dimnames(rparams) = list(unlist(options, use.names=FALSE), outputs, c())
  
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

normalize.params = function(outputs, decisions, options, dMap, params) {
  
  numOutputs = length(outputs)
  numDecis = length(decisions)
  numOpts = length(unlist(options, use.names=FALSE))
  dPat = rep(seq_along(dMap + 1), times=diff(c(dMap + 1, numOpts + 1)))
  
  L = list()
  L$min = array(NA, dim=c(numDecis, numOutputs))
  L$max = array(NA, dim=c(numDecis, numOutputs))
  
  colnames(L$min) = outputs
  colnames(L$max) = outputs
  
  for (i in 1:numOutputs) {
    M = params[,i,]
    Mx = split(M, dPat)
    L$max[,i] = unlist(lapply(Mx, FUN=max))
    L$min[,i] = unlist(lapply(Mx, FUN=min))
  }
  
  return(L)
}

rowMax = function(m) {
  apply(m, 1, max)
}

colMax = function(m) {
  apply(m, 2, max)
}

#############################################################################################
evpi = function(param, nbs, ...){
  # Author: EL
  # if parameter is not stochastic, return 0 else compute evpi
  if(length(unique(param))==1) {return(0)}
  evppi.func(param, nbs, ...)$evppi
}

######evppi.func##############################################################################################
# EL: this is copy-pasted from BCEA
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