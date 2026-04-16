#
# Basic use of the package quantreg.R
#
# - linear quantile regression 
# - lasso on linear terms
#  
# Notes
# 
# - Generating data from logstd=b'x implies that std and therefore quantiles are non-linear in x.
# - See the quantreg package manual for many more options. In particular ...
# - ... there are several optimization methods available for rq(see rq(method=...). The default is recommended for n<=5000 and p<=20, but will become slow for faster methods.
#
# Suggested exercise: compare with the percentiles from a gamlss model. 
# 


# User's options

tau = 0.05    # e.g. 0.5 for median, 0.1 for fist quantile 

n = 5000
p = 2        # p>1. Number of variables  


# end user's options

library(quantreg)
library(splines)

# Simulate data
set.seed(123)

bm = 0.5     # conditional mean coeff
bh = 0.33    # conditional log std coeff (e.g. 0.25 for moderate heteroskedasticity, 0.33 strong heteroskedasticity )

# Generate data 
x      = matrix(rnorm(n*p),n,p)   # a (n,p) matrix of independent normal
x1     = x[,1]
x2    =  x[,2]
logstd = 0.0 + bh*x1
stdtrue = exp(logstd)
Eyx     = bm*x1      # true expected value of y given x
y       = Eyx + stdtrue*rnorm(n)

# plot data
plot(x1,y, type = 'p',main = "heteroskedastic data",col = "black")

# Fit quantile regression model
r  = rq(y ~ x, tau = tau)
r2 = rq(y ~ x, tau = 1-tau)
summary(r)

q_fit = predict(r)    # can be used for fitted values and for predictive values using newdata=... )
q_fit2 = predict(r2)    # can be used for fitted values and for predictive values using newdata=... )

plot(x1,y, type = 'p',main = "heteroskedastic data",col = "black")
lines(x1,type = 'p',q_fit, col = "blue")
lines(x1,type = 'p',q_fit2, col = "blue")

# Lasso for linear terms are "experimental"
#r = rq(y ~ x, tau = tau, method = "lasso")

# The fit is not sufficiently flexible with linear terms. quantreg offers in-built splines, but to keep '
# to what we know, we can use a (less efficient) quadratic term

data = data.frame(y=y,x1=x1,x2=x1^2)
r = rq(y ~ x1 + x2, data = data, tau = tau)
r2 = rq(y ~ x1 + x2, data = data, tau = 1-tau)
summary(r)

q_fit = predict(r)    # can be used for fitted values and for predictive values using newdata=... )
q_fit2 = predict(r2)    # can be used for fitted values and for predictive values using newdata=... )

plot(x1,y, type = 'p',main = "heteroskedastic data",col = "black")
lines(x1,type = 'p',q_fit, col = "blue")
lines(x1,type = 'p',q_fit2, col = "blue")




