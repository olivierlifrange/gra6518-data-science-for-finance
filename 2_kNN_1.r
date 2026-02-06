#
# Simple example of kNN
# 
# Data is simulated from y = f(x1) + s*u, where u is N(0,1), x1 is N(0,1), and f(x1) is a possibly non-linear function set by the user # The kNN if fit at a single prediction origin xf on the training sample (y,x), where x1 is the first column of x.
# p > 1 useful to study the curse of dimensionality
#


# setwd('C:/Users/a1810185/Documents/Course Data Science/R code')    # your own path to where the kNNfunctions.r file is
source('2_kNNfunctions.r')

# User's options 

# Options for simpulated data

xf1     = 0.0               # forecast origin for the first feature (if p > 1 below, other features are set at zero) 

n       = 1000              #  number of observations in each sample
p       = 2                 #  number of features (covariates). Only the first is relevant for the DGP. p > 1 will decrease the performance of kNN

s       = 1.0                #  error standard deviation in DGP: y = f(x) + s*u, where u is N(0,1)
which_f = 2                  #  which function to simulate: 1 for linear, 2 for quadratic, 3 for logistic, 4 for cosine
bet     = 2                  #  coefficient (e.g. slope for linear). The higher, the more nonlinearity.

set.seed(123)               #  comment this line out to get different draws for the same values of the other parameters

# Options for the kNN algorithm

#k       = ceiling(0.1*n)    #   k in kNN. ceiling (0.1*n) is 10% of all data
k       = 50    #   k in kNN. 

dist    = 1                #   1 Manhattan (absolute values), 2 for Euclidean distance (default)

# End of user's options 

# simulate data
x      = matrix(rnorm(n*p),nrow = n,ncol = p)   # form a (n,p) matrix of iid N(0,1)
x1     = x[,1]
fx1    = DGPknnSim(x1,which_f,bet)  # compute f(x) = E(y|x)
y      = fx1 + s*rnorm(n)            # draw y from y = f(x) + s*u    

# create xf and fit kNN

if (p==1){xf = xf1} else {xf     = c(xf1,matrix(0,p-1) ) }

knnObject = kNNfitPG(dist,k,x,y,xf)  #  fit kNN to the sample (y,x), with forecast origin xf. 

# knnObject is a list (tuple in Python) containing multiple outputs that can be extracted as knnObject$Name
Ey_f = knnObject$eyf                # extract the kNN estimate of E(y|xf)  (the true E(y|x) is fx1Plot[t])
y_f  = knnObject$yf                  # the sample observations that are averaged
xf_f = knnObject$xff

print(" The predicted value of y at xf is ")
print(Ey_f)

# plot 

plot(x1,y, col = 'gray', xlim = c(-3,3), main = "k points in nearest neighbor (in red) " )      
points(x = xf[1], y = Ey_f, col = 'blue', pch = 19, cex = 1.5)   # kNN prediction
if(p==1)
{
  points(x = xf_f, y = y_f, col = 'red', pch = 19, cex = 1.5)   
} else
{
  points(x = xf_f[,1], y = y_f, col = 'red', pch = 19, cex = 1.5)  
}

