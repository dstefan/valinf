# VMRA (Value for Money and Risk Analysis)
# 
# Given a set of options with alternative uncertain costs and values (expressed in the same unit)
# the package computes the expected net benefit (i.e. value - cost) for all options, identifies
# the option with the highest expected net benefit and computes its risks of not achieving a 
# threshod value. It also computes overall value of information and value of information for cost
# and value of the different options.
#
# Imputs
# 1. value: a M x N matrix containing N value simulations for N alternative options
# 2. cost: a M x N matrix containing N value simulations for N alternative options (default 0)
# 3. must: a value of net benefit (i.e. value - cost) below which the project would be considered a failure
# 4. labels: a vector naming the alternative options 
# Ouputs
# Computes net benefits, risk of not meeting the must value and the expected loss,
# and value of information.
# TO DO: better presentation of outputs
vmra = function(value, cost=0, must=0, labels=NULL){
  
    N = nrow(value) # number of simulations
    M = ncol(value) # number of options
  
    if(is.null(labels)){labels = paste("Option", 1:M)}
    colnames(value) = labels
    
    # Net Benefits
    NB = value-cost
    ENB = colMeans(NB)
    # Finding the option maximising ENB
    best = which.max(ENB)
    bestENB = ENB[best]
    
    # Computing ROIs
    ROI = NB/cost
    EROI = colMeans(ROI)

    # Computing Risks
    is.failure = NB<must
    Loss = (NB-must) * is.failure
    risk = colMeans(is.failure)
    expected.loss = colSums(Loss)/colSums(is.failure) #could be NaN, division by zero
    safest = which.min(risk)
    # risks associated with option with best expected net benefit
    best.failure.likelihood = risk[best]
    best.expected.failure.magnitude = expected.loss[best]
    
    # Computing Value of Information
    
    # Overall Perfect Information
    ENB.with.all.info = mean(rowMax(NB))
    EVPI.overall = ENB.with.all.info - bestENB
    # Note: with overall perfect info, risk of failure and expected loss are reduced to zero
    # cosmetic: just to remove name on EVPI result
    names(EVPI.overall) = NULL
    
    # Labels for rows of EVPI matrixes
    evpi.labels = c("Net Benefit Increase", "Impact on Failure Risk", "Impact on Expected Loss")
    
    # Perfect Information about Costs
    EVPI.cost = matrix(nrow=3, ncol=M)
    rownames(EVPI.cost) = evpi.labels 
    colnames(EVPI.cost) = labels 
    for (i in 1:M){
      results.with.perfect.info = brpi(cost[,i], NB, must) #brpi = Benefits and risks with perfect information
      EVPI.cost[1,i] =  results.with.perfect.info$NB - bestENB
      EVPI.cost[2,i] = results.with.perfect.info$failure.risk - best.failure.likelihood
      EVPI.cost[3,i] = -(results.with.perfect.info$expected.loss - best.expected.failure.magnitude)
    }
# Assert: new version result[1,i] == old version result[i] (TODO: write tests to check this...)
    
  
    # Perfect Information about Values
    EVPI.value = matrix(nrow=3, ncol=M)
    rownames(EVPI.value) = evpi.labels 
    colnames(EVPI.value) = labels 
    for (i in 1:M){
      results.with.perfect.info = brpi(value[,i], NB, must) #brpi = Benefits and risks with perfect information
      EVPI.value[1,i] =  results.with.perfect.info$NB - bestENB
      EVPI.value[2,i] = results.with.perfect.info$failure.risk - best.failure.likelihood
      EVPI.value[3,i] = - (results.with.perfect.info$expected.loss - best.expected.failure.magnitude)
    }
    
    # To check whether new method is consitent with old one
    old.EVPI.value = rep(NA, M)
    names(old.EVPI.value) = labels
    for (i in 1:M){
      old.EVPI.value[i] = evpi(value[,i], NB)
    }
    
    
    # Note: We could also add EVPI about cost differences, value differences between solutions
    # instead of EVPI about absolute cost and values !
    
    # producing the result
    result = list(best=best, bestENB=bestENB, ENB=ENB, EROI=EROI, 
                  safest=safest, risk = risk, expected.loss=expected.loss,
                  EVPI.overall=EVPI.overall, EVPI.cost=EVPI.cost, EVPI.value=EVPI.value,
                  old.EVPI.value = old.EVPI.value
                  )
    return(result) 
}

rowMax = function(m){apply(m, 1, max)}
colMax = function(m){apply(m, 2, max)}

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
      ###  		message(paste('Fitting ',nSegs[i,j],' segmentation points for decisions ',i,j))
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

##########################################################################

###### brpi - benefit and risk with perfect information ##########################
brpi <- function(param, nbs, must,...) {
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
    
    # loop itenration over each segment
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
      failure.risk = Nbr.of.failure.with.perfect.info / n,
      expected.loss = Sum.of.loss.with.perfect.info / Nbr.of.failure.with.perfect.info,
      segPoints=param[segPoints2[-c(1,length(segPoints2))]]
      ))
  }
  else #no seg point -> no decision change, evpi = 0
    return(list(NB=NA, failure.risk=NA, expected.loss=NA, segPoints=NA))   
}

##########################################################################


# Example of Use

# simulations of N cost and value for two alternative options
N = 10^5

option1.cost = rnorm(N, 3, 1.2)
option1.value = rnorm(N, 5.5, 2.7)

option2.cost = rnorm(N, 3, 0.1)
option2.value = rnorm(N, 4, 1)

do.nothing.cost = rep(0, N)
do.nothing.value = rep(0, N)


# optionA.cost = rtriangle(N, 1, 5, 2)
# optionA.value = rtriangle(N, 0, 10, 2)


cost = cbind(option1.cost, option2.cost)
value = cbind(option1.value, option2.value)

# cost = cbind(option1.cost, do.nothing.cost)
# value = cbind(option1.value, do.nothing.value)

result = vmra(value, cost)
print(result)

