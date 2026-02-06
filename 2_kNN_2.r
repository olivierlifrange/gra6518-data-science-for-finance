#
# Generate simulated data to understand the behavior of kNN 
# 
# Data is simulated from y = f(x1) + s*u, where u is N(0,1), x1 is N(0,1), and f(x1) is a possibly non-linear function set by the user.

# The kNN if fit at a single prediction origin xf on the training sample (y,x), where x1 is the first column of x.
# p > 1 useful to study the curse of dimensionality

# The kNN estimate of E(y|xf) is then plotted --- together with the true E(y|xf) -- for a subset of the observations.
# The training sample for kNN is (y,x), where x1 is the first column of x. 
# x1 is the first column of x 
# 


# setwd('C:/Users/a1810185/Documents/Course Data Science/R code')    # your own path to where the kNNfunctions.r file is
source('2_kNNfunctions.r')   


# User's options 

# Options to generate simulated data
n       = 10000               #  number of observations in the sample
p       = 2                  #  number of features (covariates). Only the first is relevant for the DGP. p > 1 will decrease the performance of kNN

s       = 0.2                #  error standard deviation in DGP: y = f(x) + s*u, where u is N(0,1)
which_f = 4                  #  which function to simulate: 1 for linear, 2 for quadratic, 3 for logistic, 4 for cosine
bet     = 4                  #  coefficient (e.g. slope for linear). The higher, the more nonlinearity.
set.seed(1234)               #  comment this line out to get different draws for the same values of the other parameters

# Options for the kNN algorithm

#k       = ceiling(0.1*n)    #   k in kNN. ceiling (0.1*n) is 10% of all data
k       = 50    #   k in kNN. ceiling (0.1*n) is 10% of all data

dist    =  2                #   1 Manhattan (absolute values), 2 for Euclidean distance (default)


# Options for plotting (each point requires computing kNN for that point, hence nplot < n will speed up plotting)
stdplot  = 2                 # 2 to plot E(y|xf) for xf between -2 and 2
nplota   = 200               # approximate number of points for plot. If nplot and n are both large, plotting can take some times

# End of user's options 


x      = matrix(rnorm(n*p),nrow = n,ncol = p)   # form a (n,p) matrix of iid N(0,1)
x1     = x[,1]

fx1    = DGPknnSim(x1,which_f,bet)  # compute f(x) = E(y|x)
y      =  fx1 + s*rnorm(n)           # draw y from y = f(x) + s*u    

xPlot = seq(0,nplota,1)*2*stdplot/nplota - stdplot  # values of xf at which to estimate E(y|xf)
nplot = nplota +1                               # actual number of points to plot

fx1Plot= DGPknnSim(xPlot,which_f,bet)  # corresponding E(y|x) for the points to plot

EyPlot = matrix(0,nplot)              # matrix to store the kNN estimate of E(y|xf) evaluated at each point of the plot

# loop over nplot points to plot and fit kNN on the sample (y,x), which does not change, for varying forecast origin xf 
for(i in 1:nplot){
  
  if (p==1){xf = xPlot[i]} else{xf     = c(xPlot[i],matrix(0,p-1) ) } # forecast origin xf. We want an estimate of E(y|xf)
  
  knnObject = kNNfitPG(dist,k,x,y,xf)  # KEY LINE: fit kNN to the sample (y,x), with forecast origin xf. 
  # knnObject is a list (tuple in Python) containing multiple outputs that can be extracted as knnObject$Name. See the function for more.
  Ey_f = knnObject$eyf                # extract the kNN estimate of E(y|xf)  (the true E(y|x) is fx1Plot[t])

  EyPlot[i] = Ey_f
}

# Plot the results. Here we include the true E(y|xf) together with a plus-minus 2 std interval 
plotData = function(){
  plot(x[,1],y, col = 'gray',xlim = c(-2,2), main = "kNN fit on simulated data")      #sample values of y
  lines(xPlot,EyPlot, col = 'blue', lwd = 3)    #predicted values of y
  lines(xPlot,fx1Plot, col = 'red', lwd = 2)  # true E(y|x)
  
  lines(xPlot,fx1Plot + 2*s, col = 'orange', lwd = 1)     #"95%" confidence interval around fx1
  lines(xPlot,fx1Plot - 2*s, col = 'orange', lwd = 1)     #"95%" confidence interval around fx1
  legend(-4.0,-1, cex = 0.8,
         legend = c('simulated y','predicted y', 'E(y|x)', '95% confidence'),
         col = c('gray', 'blue', 'red','orange','orange'), lty = c(NA,1,1), pch = c(19,NA,NA))
}

plotData()

