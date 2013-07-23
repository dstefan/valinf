
evalDec = function(goalName, x) {
  #
  # Evaluate decisions' impact on the given goal.
  #
  # Args:
  #   goalName: A string identifying the goal for which to compute the decisions' impact.
  #   x: A vector that identifies chosen options for the decisions.
  #
  # Returns:
  #   The impact of the decision vector on the given goal.
  #
  
  # Find goal's column index.
  gi = which(gol == goalName)
  # Fiter out simulated values for the given goal and decision vector.
  rows = sim[x + altFilter,gi,]
  
  if (length(dec) == 1) {
    res = rows
  }
  else {
    # If more than one decision, we must assign new variable for each of them
    # and evaluate aggregate using decFx corresponding to the given goal
    for (i in 1:length(dec)) {
      assign(dec[i], rows[i,])
    }
    res = eval(parse(text = decFx[gi]))
  }
  
  return(res)
}


evalBen = function(x) {
  #
  # Evaluate the benefit of a solution given decision vector x.
  #
  # Args:
  #    x: A vector that identifies the decisions.
  # 
  # Returns:
  #    The overall benefit of the solution.
  #
  
  # Assign new variable for each goal and instantiate it with the impact of the decision.
  for (i in 1:length(gol)) {
    assign(gol[i], evalDec(gol[i], x))
  }
  # Evaluate the benefit function
  res = eval(parse(text = benFx))
  
  return(res)
}

NBAll = function(X) {
  #
  # Compute the benefit distribution of given solutions.
  #
  # Args:
  #    X: A set of vectors where each vector identifies one solution.
  #
  # Returns:
  #    A matrix with columns equal to the simulated benefit distributions of given solutions.
  #
  
  NB = matrix(nrow = N, ncol = length(X));
  colnames(NB) = paste(X)
  for (i in 1:length(X)) {
    NB[,i] = evalBen(X[[i]])
  }
  return(NB)
}


goalAll = function(goalName, X) {
  #
  # Compute the value distribution of a goal given a set of solutions.
  #
  # Args:
  #    goalName: The name of the goal.
  #    X: A set of vectors where each vector identifies one solution.
  # 
  # Returns:
  #    A matrix where each column equals the value distributions for given goal.
  #
  
  G = matrix(nrow = N, ncol = length(X));
  colnames(G) = paste(X)
  for (i in 1:length(X)) {
    G[,i] = evalDec(goalName, X[[i]])
  }
  return(G)
}


evpiAll = function(NB, X) {
  #
  # Test function to compute the evpi for all goals.
  #
  
  res = array(dim = c(length(gol), length(X)))
  dimnames(res) = list(gol, paste(X))
  
  for (i in 1:length(gol)) {
    G = goalAll(gol[i], X)
    for (j in 1:ncol(G)) {
      res[i,j] = evpi(G[,j], NB)
    }
  }
  
  return(res)
}


evpi = function(param, nbs, ...){
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