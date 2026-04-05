#
# Simple example of Principal Components Regression on simulated data
# See James et al. 6.7.1 for an application to real data, and for how to perform cross-validation
#
#

library(pls)                   # first install the library. Tools -- > Install Packages. glmnet requires version >= 3.6

set.seed(1234)

# dgp

n      = 5000                       # sample size of simulated dataset
s      = 1.0                       # std of errors. Determines the signal-to-noise
p      = 10

afactor = 0.5                    # 0<=afactor<1. afactor = 0 for uncorrelated features, > 0 for positively correlated features

# The simulated data here share a common factor, which we can use to produce correlated features

xc     = rnorm(n)
xcommon= matrix(xc,nrow = n,ncol = p)          # repeat the same rnom(n) vector across p columns
xnoise = matrix(rnorm(n*p),nrow = n,ncol = p)

x      = afactor*xcommon + (1-afactor)*xnoise         # 0.0*xcommon + noise recovers independent features

y      = xc + s*rnorm(n)

# data are generated as y = xc + u, but we do not observe xc. We observe the (n,p) matrix x, where each column of x is xcommon + noise

df = data.frame("y" = y,"x" = x)

pcr.fit = pcr(y~x,data = df, ncomp = 1, scale = TRUE)
summary(pcr.fit)

pcr.fit = pcr(y~x,data = df, ncomp = 10, scale = TRUE)
summary(pcr.fit)

# instead of using pcr(), let's do it from scratch

# 1) standardize x 
xs    = (x - mean(x))/sd(x)

# 2) SVD decomposition of xs. xs = UDV'
s     = svd(xs)

D     = diag(s$d)
U     = s$u 
V     = s$v 

z     = xs %*% V     # z = X*V are the principal components

# compute the correlation of the true unobserved xc with the first principal component estimated from data
print(cor(xc,z[,1]))

# OLS of y on the first five principal components

df = data.frame("y" = y,"z" = z)

ls.fit = lm(y~z[,1:5],data = df)
summary(ls.fit)