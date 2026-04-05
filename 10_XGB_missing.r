#
# Illustration of how gradient boosting machines can handle missing values. 
#
# Example reproduces example1 in the paper: "On the consistency of supervised learning with missing values" by Josse et al., 2020. 
#
# Gradient boosting machines can handle missing values automatically and, asymptotically, they do so optimally. Data imputation is therefore optional. 
#
# Exercise: run with rho = 0 and then rho = 0.8, and see if you can make sense of the result. 
#
# 


library(data.table)
library(xgboost)
library(mvtnorm)

set.seed(1)

n <- 10000              # number of observations 
p <- 5                 # number of features
stde <- 0.1            # standard deviation of the errors 
rho <- 0.5             # cross-correlation of features 
prob <- 0.2            # probability of missing in 1st feature 

# Missing at random (MCAR) function
dgp_with_missing <- function(x, prob) {
  prob <- 0.2
  ind <- runif(nrow(x)) < prob
  f <- x[,1]^2
  x[ind,1] <- NA
  return(list(f, x))
}

# Generating cross-correlated data
u <- rep(1, p)
mu <- rep(0, p)
V <- rho * (u %*% t(u)) + (1 - rho) * diag(p)

x <- mvtnorm::rmvnorm(n, mu, V)

out <- dgp_with_missing(x, prob)
f <- out[[1]]
x <- out[[2]]

y <- f + stde * rnorm(n)

# Run XGB, cv the number of trees 
dtrain = xgb.DMatrix(data = x, label= y)
bst = xgb.cv(data = x,label = y,nrounds = 1000,objective = "reg:squarederror",early_stopping_rounds = 100,nfold = 5,eta=0.1,max.depth = 2,verbose=1)

# NOTE: xgb.cv does not support predict(bst,...). We must run xgboost with nrounds = as.numeric(bst[8]). bst is a list of outputs. bst[8] is the number of trees (with lowest cv loss)
bst = xgboost(data = dtrain, max.depth = 3, eta = 0.1, nthread = 2, nrounds = as.numeric(bst[8]),objective = "reg:squarederror", verbose = 0)

# Forecast at x1 = 0 and x1 = NaN 

x0   = 0*x[1:2,]         # pred seems to require a matrix with at least two elements  
x0[2,1] = NaN 

pred = predict(bst,x0)  # prediction, to be compared with y_test
print("prediction at x1=0 and x1=NaN  ")
pred
