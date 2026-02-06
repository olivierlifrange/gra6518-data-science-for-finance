

# locregfitPG
# 
# Purpose:
# Simple implementation of local regression
# 
# Note: The R package locfit performs local regression and local likelihood, but we'll be using our own simpler functions here. 
#
# Use:
# results = locregPG(Dist,K,y,xk,xreg,xkf,xregf)
# 
# Inputs:
#   
# - Dist      Distance measure. 1 for Manhattan, 2 for Euclidean
# - K         Number of observations in neighbor
# - y         (n,1) vector of realizations, training sample 
# - xk       (n,pk) matrix of features on which the distance measure is computed, training sample.
# - xreg     (n,preg) matrix of features for the linear regression (may be the same as xk) 
# - xkf      (1,pk) vector of forecast origin, distance features 
# - xregf    (1,preg) vector of forecast origin, regression features
#
# 
# Output:
#   
# - results$eyf   # (1,1) prediction, the forecast value of E(y|x)
# - results$yf    # (K,1) realizations in the nearest neighbor of xf
# - results$bf    # (preg) vector of coefficients in the OLS regression
# - results$pos   # (K,1) vector of indexes of observations in nearest neighbor
#
# Notes:
#
# - xk is standardized inside the function using (x - mean(x))/s(x), where s depends on Dist. Other forms of standardization are possible.
# - The function cannot deal with missing values. 
#

locregPG = function(Dist,K,y,xk,xreg,xkf,xregf)
{
  
  # standardize the features on which the distance is computed: good performance of kNN requires some form of standardization.
  meanxk = mean(xk)
  z  = xk - meanxk
  zf = xkf - meanxk
  
  if(Dist == 1){ s = mean(abs(z) )} else{ s = sd(xk) }
  
  z  = z/s
  zf = zf/s
  
  # features are now standardized: compute Manhattan or Euclidean distance of all features in x (now z) from xf (now zf) 
  if(Dist == 1){ d = colSums(t(abs(zf-z))) } else {d = sqrt(colSums(t(zf[1]-z)^2))}
  
  # Having computed the distance from xf of all observations, we order the observations from the closest to the furthest and pick the k closest
  xy    = cbind(d,y,xreg,1:NROW(y))                   #  stack (d,y,x,indexes) together prior to sorting
  soxy  = xy[order(xy[,1]),]                     #  order the stack based on distance (the first column of xy)
  y_f   = soxy[1:K,2]                            #  (K,1) 
  xregf_f = soxy[1:K,3:(2+NCOL(xreg))]           #   (K,p)  corresponding covariates closest in distance to xf (from x)
  pos   = soxy[1:K,NCOL(soxy)]                  #   (K,1)  positions of xf_f in x
  
  lm.fit = lm(y_f~xregf_f)
  b      = lm.fit$coeff
  

  eyf    =  b[1] + sum( b[2:NROW(b)]*xregf )
  


  # build a list for the otput  
  results = list()
  
  results$eyf = eyf
  results$yf = y_f
  results$bf = b
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
