#
#   Small program to gain some understanding of how finding the best model for forecasting is not the same
#   as finding the "true" model, and how the best model for forecasting will depend not only on the true model,
#   but also on the sample size and signal-to-noise ratio.
#
#   Paolo Giordani, October 2020

# True model is y(i) = x(i)'b + s*e(i), where e(i) is N(0,1)

set.seed(123)

n      = 1000                     # 
s      = 1.0                     # std of errors

a      = 0.2                            # a should be non-zero
b      = c(3.0,2.0,1.0,a,a,a,a,a,a,a)   # all ten features are relevant, but some are large, some are small. 


# simulate data
p      = length(b)
x      = matrix(rnorm(n*p),nrow = n,ncol = p)   # form a (n,p) matrix of iid N(0,1)

# Notice the use of %*%. x*b works in Matlab, Julia, Python, but not in R. In R, x*b is elementwise multiplication, and here gives a (n,p) matrix 
fx     =  x %*% b  # compute f(x) = E(y|x)
y      = fx + s*rnorm(n)            # draw y from y = f(x) + s*u    

# Fit a model with all the features

# compute the out-of-sample error. In this case we could do so analytically, but we are going to use a numerical solution that works more generally.
# In a simulation environment, we set n_test to a large number. 
n_test = 100000            
x_test = matrix(rnorm(n_test*p),nrow = n_test,ncol = p)
Eyx    = x_test %*% b             # true E(y|x)

df = data.frame("y" = y,
                "x" = x)

lm.fit = lm(y~x,data = df)           

bhat   = lm.fit$coefficients
yhat   = bhat[1] + x_test %*% bhat[2:NROW(bhat)]

RMSE_10 = sqrt( mean( (Eyx - yhat)^2  ) )         # notice that in simulation we can define MSE as ( Eyx - yhat)^2 


# Repeat, only include first 3 features 

lm.fit = lm(y~x[,1]+x[,2]+x[,3],data = df)           

bhat   = lm.fit$coefficients
yhat   = bhat[1] + x_test[,c(1,2,3)] %*% bhat[2:NROW(bhat)]

RMSE_3 = sqrt( mean( (Eyx - yhat)^2  ) )         # notice that in simulation we can define MSE as ( Eyx - yhat)^2 


# which model is best in out-of-sample?
print(" signal-to-noise: true var(E(y|x))/var(y) ")
print(var(Eyx)/( var( Eyx  ) + s^2))
print(" Ratio of RMSE of true model over simpler model ")
print(RMSE_10/RMSE_3)



  