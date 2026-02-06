
# 
# Validation Set Approach on global equity dataset
#
# Reference: James et al. 5.3.1, 
#
# Remark:
# We'll show the simplest approach, straight out of James et al. 5.3.1. This does not take into consideration the time series nature of the data, and that observations
# are cross-correlated. 
# 
# 
#
# Paolo Giordani, October 2020.  

set.seed(123)

# user's inputs

sharev   = 0.2     # share of the data for the validation sample, e.g. 0.2 or 0.3

#  end user's inputs 

dataGE = read.csv("C:/Users/a1810185/Documents/A_Data Science/Data/GlobalEquityReturns.csv", header = TRUE, sep = ",")
colnames(dataGE) = c('date','countrynumber','excessret','logCAPE','momentum','avgvol','vol3m','vol12m')

attach(dataGE, warn.conflicts = F)             # The database 'data' (dataframe or list) is attached to the R search path. Now we can use the variable names.

# Split the sample into two random sub-sets: training and validation. Because the assignment to each subset is completely random, there is no temporal aspect to the
# training and validation set

n       = dim(dataGE)[1]          # sample size
n_train = round((1-sharev)*n)     # number of observations in the training set: sharev*n, where 

train   = sample(n,n_train, replace = FALSE)   # draw a random sub-set of observations without replacement as the training set. This is a n_train vector of indices. 

# on the training sample, fit OLS with 3 predictors and an interaction term.
lm.fit = lm(excessret~logCAPE+momentum+vol3m + logCAPE:momentum, subset = train   )
rmse1  = sqrt(mean( ( excessret - predict(lm.fit,dataGE)  )[-train]^2   ))

# and again without volatility and the interaction term. 
lm.fit = lm(excessret~logCAPE+momentum, subset = train   )
rmse2  = sqrt(mean( ( excessret - predict(lm.fit,dataGE)  )[-train]^2   ))

# which one has lower rmse? Change seed and repeat. 
print(" rmse in validation set of model with 4 features over model with 2. Numbers lower than one favor the larger model ")
print(rmse1/rmse2)

