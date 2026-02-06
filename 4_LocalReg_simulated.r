#
# Simple example of local regression
# 
# Data is simulated from y = f(x1) + s*u, where u is N(0,1), x1 is N(0,1), and f(x1) is a possibly non-linear function set by the user # The kNN if fit at a single prediction origin xf on the training sample (y,x), where x1 is the first column of x.
# p > 1 useful to study the curse of dimensionality.
#
#
# Paolo Giordani, September 2000


source('LocalReg_functions.r')   

# User's options 

set.seed(123)               #  comment this line out to get different draws for the same values of the other parameters

# Options for simpulated data

xf1      = 1.0               # forecast origin for the first feature (if p > 1 below, other features are set at zero) 

n       = 500               #  number of observations in each sample
p       = 1                  #  number of features (covariates). Only the first is relevant for the DGP

s       = 2.5                #  error standard deviation in DGP: y = f(x) + s*u, where u is N(0,1)
which_f = 2                  #  which function to simulate: 1 for linear, 2 for quadratic, 3 for logistic, 4 for cosine
bet     = 8                  #  coefficient (e.g. slope for linear). The higher, the more nonlinearity.

set.seed(123)               #  comment this line out to get different draws for the same values of the other parameters

# Options for the kNN algorithm

K       = 100               #   number of neighbours
pd      =  1                #   the first pd features are used in computing the distance. All are used for the linear regression.

Dist    =  2                #   1 Manhattan (absolute values), 2 for Euclidean distance (default)


# End of user's options 

# simulate data
xreg   = matrix(rnorm(n*p),nrow = n,ncol = p)   # form a (n,p) matrix of iid N(0,1)
x1     = xreg[,1]
fx1    = DGPknnSim(x1,which_f,bet)  # compute f(x) = E(y|x)
y      = fx1 + s*rnorm(n)            # draw y from y = f(x) + s*u    

xk     = xreg[,1:pd]                 # distance is computed on first pd features in the traning sample

# create xf and fit local regression 

if (p==1){xregf = xf1} else {xregf     = c(xf1,matrix(0,p-1) ) }
xkf       = xregf[1:pd]    # only the first pdf features are used in computing the distance

lrObject = locregPG(Dist,K,y,xk,xreg,xkf,xregf) 


# knnObject is a list (tuple in Python) containing multiple outputs that can be extracted as knnObject$Name
Ey_f = lrObject$eyf                # extract the local reg estimate of E(y|xf)  (the true E(y|x) is fx1Plot[t])
y_f  = lrObject$yf                  # the sample observations that are averaged
xf_f = xreg[lrObject$pos,]
b_f  = lrObject$bf

print(" The predicted value of y at xf is ")
print(Ey_f)

# plot 

plot(x1,y, col = 'gray', xlim = c(-2,2), main = "k points in nearest neighbor (in red) " )      
points(x = xkf[1], y = Ey_f, col = 'green', pch = 19, cex = 5.0)  
if(p==1)
{
  points(x = xf_f, y = y_f, col = 'red', pch = 19, cex = 1.5)   
} else
{
  points(x = xf_f[,1], y = y_f, col = 'red', pch = 19, cex = 1.5)  
}


# Now plot on a grid of values of x.

# Options for plotting (each point requires computing kNN for that point, hence nplot < n will speed up plotting)
stdplot  = 2                 # 2 to plot E(y|xf) for xf between -2 and 2
nplota   = 200               # approximate number of points for plot. If nplot and n are both large, plotting can take some times

xPlot = seq(0,nplota,1)*2*stdplot/nplota - stdplot  # values of xf at which to estimate E(y|xf)
nplot = nplota +1                                   # actual number of points to plot

fx1Plot= DGPknnSim(xPlot,which_f,bet)  # corresponding E(y|x) for the points to plot

EyPlot = matrix(0,nplot)              # matrix to store the local regression estimate of E(y|xf) evaluated at each point of the plot

# loop over nplot points to plot and fit kNN on the sample (y,x), which does not change, for varying forecast origin xf 
for(i in 1:nplot){
  
  if (p==1){xregf = xPlot[i]} else{xregf     = c(xPlot[i],matrix(0,p-1) ) } # forecast origin xf. We want an estimate of E(y|xf)
  xkf = xregf[1:pd]
    
  lr = locregPG(Dist,K,y,xk,xreg,xkf,xregf) 
  
  EyPlot[i] = lr$eyf          # extract the local regression estimate of E(y|xf)  (the true E(y|x) is fx1Plot[t])
}

# Plot the results. Here we include the true E(y|xf) together with a plus-minus 2 std interval 
plotData = function(){
  plot(x1,y, col = 'gray',xlim = c(-2,2), main = "local reg fit on simulated data")      #sample values of y
  lines(xPlot,EyPlot, col = 'blue', lwd = 3)    #predicted values of y
  lines(xPlot,fx1Plot, col = 'red', lwd = 2)  # true E(y|x)
  
  lines(xPlot,fx1Plot + 2*s, col = 'orange', lwd = 1)     #"95%" confidence interval around fx1
  lines(xPlot,fx1Plot - 2*s, col = 'orange', lwd = 1)     #"95%" confidence interval around fx1
  legend(-4.0,-1, cex = 0.8,
         legend = c('simulated y','predicted y', 'E(y|x)', '95% confidence'),
         col = c('gray', 'blue', 'red','orange','orange'), lty = c(NA,1,1), pch = c(19,NA,NA))
}

plotData()

