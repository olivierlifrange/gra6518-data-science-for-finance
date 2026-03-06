#
# Exploring ridge regression and lasso on simulated data
#
# Further reference: James et al. 6.6 and the library glmnet
#
# True model is y(i) = x(i)'b + s*e(i), where e(i) is N(0,1)
#
# length(b) = 10, with the first 3 coefficients being 1.0, and all the others set to a, where a could be 0 or 1 or anything else. 
# 
# Key points to notice:
# - the amount of penalization chosen by cv depends on the signal-to-noise ratio
# - the path of coefficients as a function of lambda looks quite different for independent features and dependent features. 
# - make sure you can read these paths
#
# Paolo Giordani, October 2020


library(glmnet)                   # first install the library. Tools -- > Install Packages. glmnet requires version >= 3.6

set.seed(1)

# dgp


n      = 100000                      # sample size of simulated dataset
s      = 10.0                       # std of errors. Determines the signal-to-noise

a      = 0.2
b      = c(1.0,1.0,1.0,a,a,a,a,a,a,a)   # 10,1 vector of true parameter values

afactor = 0.7                   # 0<=afactor<1. afactor = 0 for uncorrelated features, > 0 for positively correlated features

# simulate data
p      = length(b)

# The simulated data here share a common factor, which we can use to produce correlated features

xcommon= matrix(rnorm(n),nrow = n,ncol = p)
xnoise = matrix(rnorm(n*p),nrow = n,ncol = p)

x      = afactor*xcommon + (1-afactor)*xnoise         # 0.0*xcommon + noise recovers independent features
print(cor(x))


# Notice the use of %*%. x*b works in Matlab, Julia, Python, but not in R. In R, x*b is elementwise multiplication, and here gives a (n,p) matrix 
fx     =  x %*% b  # compute f(x) = E(y|x)
y      = fx + s*rnorm(n)            # draw y from y = f(x) + s*u    
SNR    = sum(b^2)/( sum(b^2)  + s^2 )     # a measure of the signal-to-noise ratio: var( E(y|x))/var(y)

print(" var(E(y|x))/var(y) -- a measure of the signal-to-noise ratio -- is  ")
print(SNR)


# ols with a ridge regression (alpha = 0) function glmnet. Here we fix lambda to a tiny number, which is the same as ols. 
ols.mod  = glmnet(x,y,alpha=0,lambda = 10^-10, standardize = TRUE) #Fit one ridge model with λ = 0.01
print(coef(ols.mod))

# if we don't specify lambda (or a grid for lambda), an extensive grid from very large values to very small values is fit
ridge.mod  = glmnet(x,y,alpha=0,standardize = TRUE) # Fit many ridge models across λ grid
plot(ridge.mod)                 # plot all coeff on a grid 
#print(coef(ridge.mod))         #  big output. Suppress is not needed.

# cross-validation to choose lambda.
cv.out  = cv.glmnet(x,y,alpha = 0, nfolds = 10)
plot(cv.out)

best_lambda = cv.out$lambda.min
print(" best lambda in cv for ridge is ")
print(best_lambda) 

# compare coefficients with the best lambda to ols 
ridgebestlambda.mod  = glmnet(x,y,alpha=0,lambda=best_lambda,standardize = TRUE)
print(coef(ols.mod))
print(coef(ridgebestlambda.mod))



# Lasso.

# Entire lasso path 
lasso.mod  = glmnet(x,y,alpha=1,standardize = TRUE)
plot(lasso.mod)                 # plot all coeff on a grid 


# cv 
cv.out  = cv.glmnet(x,y,alpha = 1, nfolds = 10)
plot(cv.out)

best_lambda = cv.out$lambda.min
print(" best lambda in cv for lasso is ")
print(best_lambda) 

lassobestlambda.mod  = glmnet(x,y,alpha=0,lambda=best_lambda,standardize = TRUE)
print(coef(ols.mod))
print(coef(lassobestlambda.mod))

      
      
