#
# Generates data from a linear regression with skewed errors.
# 
# If the objective function (loss function) is "reg:squarederror", the model computes the 
# conditional mean, and it is unbiased, but if the objective function is "reg:absoluteerror"
# the model computes the conditional median, and the forecasts are biased.  
#
# Paolo Giordani, March 2024

library(xgboost)

set.seed(123)

# set loss functions ("ojective")
# "reg:absoluteerror" for MAE, or "reg:squarederror" for MSE
objective = "reg:absoluteerror"   

# Create simulated data or import data 
n      = 10000  # training sample size

# simulate data 
# generate f(x) = E(y|x)
b   = c(1.0,1.0,1.0,0,0,0,0,0,0,0)   # 10,1 vector of true parameter values

p       = length(b)
x_train = matrix(rnorm(n),nrow = n,ncol = p)
f_train = x_train %*% b # compute f(x) = E(y|x). 

# generate skewed iid errors as a mixture of two normals

stde      = 5
m2        = 3*stde    # mean of second component of the mixture. 
stde2     = 3*stde    # std of second component of the mixture.
prob2     = 0.3       # probability of the second component

u1 = rnorm(n) * stde
u2 = m2 + rnorm(n) * stde2

S1 = runif(n) > prob2
u = u1 * S1 + u2 * (1 - S1) - prob2 * m2

# Compute y
y_train = f_train + u

# Plot histogram of u
hist(u, main = "errors", xlab = "", ylab = "Frequency", col = "lightblue", border = "black")

# Standard matrices are accepted, but the class-specific DMatrix is recommended (and required for some of the most advanced features.)
dtrain = xgb.DMatrix(data = x_train, label= y_train)

# For n-fold CV, use xgb.cv. Notice the default is randomized cv.
bst = xgb.cv(data = dtrain,
            objective = objective, 
            nrounds = 1000,
            early_stopping_rounds = 200,
            nfold = 5,
            eta=0.1,
            max.depth = 6,
            verbose=0)

# NOTE: xgb.cv does not support predict(bst,...). We must run xgboost with nrounds = as.numeric(bst[8]). bst is a list of outputs. bst[8] is the number of trees (with lowest cv loss)
bst = xgboost(
    objective = objective, 
    data = dtrain, max.depth = 6,
    eta = 0.1,
    nrounds = as.numeric(bst[8]),
    verbose = 0)

pred = predict(bst, x_train)  # prediction, to be compared with y_test


bias = mean( y_train - pred )

print(paste("bias ", bias))
print(paste("bias divided by std of the errors", bias/sd(u)))
