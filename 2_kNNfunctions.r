#
# This file contains two functions:
#
# 1) kNNfitPG is a simple implementation of k-nearest neighbor. More efficient and complex options are available from R packages, but
#    this is a good place to start to understand the algorithm
#
# 2) DGPknnSim simulates data from a variety of linear and non-linear functions
# 
#
# Paolo Giordani, August 2020
#



# kNNfitPG
# 
# Purpose:
# Simple implementation of k nearest neighbor
# 
# Use:
# results = kNNfitPG(Dist,K,x,y,xf)
# 
# Inputs:
#   
# - Dist      Distance measure. 1 for Manhattan, 2 for Euclidean
# - K         Number of observations in neighbor
# - x         (n,p) matrix of features, training sample
# - y         (n,1) vector of realizations, training sample 
# - xf        (p,1) vector of forecast origin 
# 
# Output:
#   
# - results$eyf   # (1,1) prediction, the forecast value of E(y|x)
# - results$yf    # (K,1) realizations in the nearest neighbor of xf
# - results$xff   # (K,p) features        "
# - results$pos   # (K,1) vector of indexes of observations in nearest neighbor
#
# Notes:
#
# - x is standardized inside the function using (x - mean(x))/s(x), where s depdends on Dist. Other forms of standardization are possible.
# - The function cannot deal with missing values. The extension is not difficult though. 
#

kNNfitPG = function(Dist,K,x,y,xf)
{

  # standardize the features: good performance of kNN requires some form of standardization.
  z  = x - mean(x)
  zf = xf - mean(x)
  
  if(Dist == 1){ s = mean(abs(z) )} else{ s = sd(x) }
  
  z  = z/s
  zf = zf/s

  # features are now standardized: compute Manhattan or Euclidean distance of all features in x (now z) from xf (now zf) 
  if(Dist == 1){ d = colSums(t(abs(zf-z))) } else {d = sqrt(colSums(t(zf[1]-z)^2))}

  # Having computed the distance from xf of all observations, we order the observations from the closest to the furthest and pick the k closest
  xy    = cbind(d,y,x,1:NROW(y))                   #  stack (d,y,x,indexes) together prior to sorting
  soxy  = xy[order(xy[,1]),]                     #  order the stack based on distance (the first column of xy)
  y_f   = soxy[1:K,2:(1+NCOL(y))]                  #   (K,py) all observations whose average give Eyf
  xf_f  = soxy[1:K,(2+NCOL(y)):(NCOL(soxy)-1)]    #   (K,p)  corresponding covariates closest in distance to xf (from x)
  pos   = soxy[1:K,NCOL(soxy)]                     #   (K,1)  positions of xf_f in x
  
  eyf = mean(y_f)                               # prediction: mean of y in n.n.

  # build a list for the otput  
  results = list()

  results$eyf = eyf
  results$yf = y_f
  results$xff = xf_f
  results$pos = pos
  
  return(results)
}

# DGPknnSim
#
# Inputs
# - x1          is a (n,1) vector 
# - which_f     which function to simulate: 1 for linear, 2 for quadratic, 3 for logistic, 4 for cosine
# - bet         coefficient (e.g. slope for linear). The higher, the more nonlinearity.
#
# Output: a (n,1) vector of E(y|x1) = f(x1;bet)


DGPknnSim = function(x1,which_f,bet){

  if(which_f == 1)
  {
    fx1 = bet*x1
  } else if(which_f == 2)
  {
    fx1 = bet*x1^2
  } else if(which_f == 3)
  {
    fx1 = 1/(1+exp(-bet*x1))
  } else if(which_f == 4)
  {
    fx1 = cos(bet*x1)
  } else
  {
    print('which_f can only take integer values from 1 to 4')
    return(NA)
  }
  
  return(fx1)
  
  
  
}

