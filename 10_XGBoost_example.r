#
# Illustration of some of the most basic functions of XGBoost
#
# Some optional additional resources on XGBoost for R
# - Intro to xgboost for R: https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html 
# - Manual in pdf: https://cran.r-project.org/web/packages/xgboost/xgboost.pdf    
# - XGBoost Parameters: https://xgboost.readthedocs.io/en/latest/parameter.html
# - Official GitHub page: https://github.com/dmlc/xgboost
#
#
#
#
# Recommended exercises
# 1) With simulated data, try different combination of sample size and SNR (change n and s) as well as different dgp
# 2) Repeat for the GlobalEquity dataset or some other data 
# 3) Cross-validate tree depth (max.depth)
#

library(xgboost)

set.seed(123)

# Create simulated data or import data 
n      = 10000  # training sample size
n_test = 10000   # simulated sample size
s      = 1.0    # standard deviation of errors 

# simulate data 
b   = c(1.0,1.0,1.0,0,0,0,0,0,0,0)   # 10,1 vector of true parameter values

p       = length(b)
x_train = matrix(rnorm(n),nrow = n,ncol = p)
x_test  = matrix(rnorm(n_test),nrow = 100000,ncol = p)  

alpha   = 1.0   # Let's first try ^1.0 -- linear--, then ^2.0 -- very complex nonlinear function                 
f_train = (x_train %*% b)^alpha  # compute f(x) = E(y|x). 
f_test  = (x_test %*% b)^alpha 

y_train = f_train + s*rnorm(n)          
y_test  = f_test  + s*rnorm(n_test)


# basic xgboost, with pre-determined nrounds and depth, on dense matrices  
bst = xgboost(data = x_train, label = y_train, max.depth = 6, eta = 0.1, nthread = 2, nrounds = 20, verbose = 1,objective = "reg:squarederror")
#pred = predict(bst, x_test)  # prediction, to be compared with y_test

# Standard matrices are accepted, but the class-specific DMatrix is recommended (and required for some of the most advanced features.)
dtrain = xgb.DMatrix(data = x_train, label= y_train)
dtest =  xgb.DMatrix(data = x_test, label=y_test)

# xgboost.train has early stopping. Useful when we have ONE test set 
watchlist <- list(train=dtrain, test=dtest)  
bst = xgb.train(data=dtrain, max.depth=4, eta=0.1, nthread = 2,nrounds=1000,early_stopping_rounds = 40, objective = "reg:squarederror",watchlist=watchlist)
pred = predict(bst, x_test)  # prediction, to be compared with y_test

# For n-fold CV, use xgb.cv. Notice the default is randomized cv.
bst = xgb.cv(data = dtrain,nrounds = 1000,objective = "reg:squarederror",early_stopping_rounds = 20,nfold = 5,eta=0.1,max.depth = 3,verbose=1)
err =  sqrt(mean( (y_test - pred)^2)) 
print(paste("RMSE ", err))

#We can also use xbg.train and xgb.cv with dense matrices (not DMatrix).
#bst = xgb.cv(data = x_train,label = y_train,nrounds = 1000,objective = "reg:squarederror",early_stopping_rounds = 20,nfold = 5,eta=0.1,max.depth = 2,verbose=0)

# NOTE: xgb.cv does not support predict(bst,...). We must run xgboost with nrounds = as.numeric(bst[8]). bst is a list of outputs. bst[8] is the number of trees (with lowest cv loss)
bst = xgboost(data = dtrain, max.depth = 3, eta = 0.1, nthread = 2, nrounds = as.numeric(bst[8]),objective = "reg:squarederror", verbose = 1)
#bst = xgboost(data = x_train, label = y_train, max.depth = 2, eta = 0.1, nthread = 2, nrounds = as.numeric(bst[8]),objective = "reg:squarederror", verbose = 0)
pred = predict(bst, x_test)  # prediction, to be compared with y_test

err = mean( abs(y_test - pred) )
print(paste("MAE  ", err))

err =  sqrt(mean( (y_test - pred)^2)) 
print(paste("RMSE ", err))

# the following can be computed only for simulated data
err =  sqrt(mean( (f_test - pred)^2)) 
print(paste("RMSE f_test - pred ", err))

# Importance matrix (requires feature names)
#importance_matrix <- xgb.importance(names, model = xgb) 
#print(importance_matrix)


