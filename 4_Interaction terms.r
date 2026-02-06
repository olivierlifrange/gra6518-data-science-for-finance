
# 
# Predicting international stock market returs: interaction terms.
#
# See OLS.r for a description of the data and of some of the functions
#
#
# Paolo Giordani, September 2020.  


dataGE = read.csv("C:/Users/a1810185/Documents/A_Data Science/Data/GlobalEquityReturns.csv", header = TRUE, sep = ",")
colnames(dataGE) = c('date','countrynumber','excessret','logCAPE','momentum','avgvol','vol3m','vol12m')
attach(dataGE, warn.conflicts = F)             # The database 'data' (dataframe or list) is attached to the R search path. Now we can use the variable names.

# OLS with 3 predictors and an interaction term. The interaction term is written x1:x2
lm.fit = lm(excessret~logCAPE+momentum+logCAPE:momentum + vol3m   )

# We could also use the short-hand R syntax x1*x2, which includes x1,x2 and x1:x2
#lm.fit = lm(excessret~logCAPE*momentum  )

# Notice how R insists on placing the interaction term last, even if in our specification vol3m is last 
print(summary(lm.fit))          
b      = coef(lm.fit)           

# From the slides: b1*x1 + b2*x2 + b3*x1*x2 + b4*x3 can be written as (b1 + b3*x2)*x1 + b2*x2 + b4*x3. This can be seen as a time-varying 
# coefficient on x1, b1(x2) = b1 + b3*x2. Let's compute and plot b1(x2) on a grid of values of x2 (momentum) in sample 

# Be careful with the indexes: b[1] is the intercept and the interaction term is placed last

x2       = seq( from = -2*sd(momentum)+mean(momentum), to = 2*sd(momentum)+mean(momentum), length.out = 100 )
b1_of_x2 = b[2] + b[5]*x2

plot(x2,b1_of_x2, main = 'coefficient on valuation as a function of momentum', xlab = 'momentum', ylab = 'coefficient on logCAPE', col = 'red')

# We can do the same thing for momentum as a function of valuation:  b1*x1 + b2*x2 + b3*x1*x2 + b4*x3 can also be written as b1*x1 + (b2 + b3*x1)*x2
x1       = seq( from = -2*sd(logCAPE)+mean(logCAPE), to = 2*sd(logCAPE)+mean(logCAPE), length.out = 100 )
b2_of_x1 = b[3] + b[5]*x1

plot(x1,b2_of_x1, main = 'coefficient on momentum as a function of valuation', xlab = 'logCAPE', ylab = 'coefficient on momentum', col = 'red')

# repeat, but now interact logCAPE with vol3m
lm.fit = lm(excessret~logCAPE+vol3m+logCAPE:vol3m + momentum   )
print(summary(lm.fit))          

b        = coef(lm.fit)
x2       = seq( from = -2*sd(vol3m)+mean(vol3m), to = 2*sd(vol3m)+mean(vol3m), length.out = 100 )
b1_of_x2 = b[2] + b[5]*x2

plot(x2,b1_of_x2, main = 'coefficient on valuation as a function of vol3m', xlab = 'vol3m', ylab = 'coefficient on logCAPE', col = 'red')


# cross-products generate features with heavy tails
hist(logCAPE*momentum, breaks = 100)
hist(logCAPE*vol3m, breaks = 100)



