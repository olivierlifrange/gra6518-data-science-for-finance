
# 
# Predicting international stock market returs: comparing OLS and WLS.
#
# See OLS.r for a description of the data and of some of the functions
#
# Paolo Giordani, September 2020.  


dataGE = read.csv("C:/Users/a1810185/Documents/A_Data Science/Data/GlobalEquityReturns.csv", header = TRUE, sep = ",")
colnames(dataGE) = c('date','countrynumber','excessret','logCAPE','momentum','avgvol','vol3m','vol12m')

# OLS with 3 predictors
attach(dataGE, warn.conflicts = F)             # The database 'data' (dataframe or list) is attached to the R search path. Now we can use the variable names.
lm.fit = lm(excessret~logCAPE+momentum+vol3m)

print(summary(lm.fit))          
b_ols = coef(lm.fit)            

# countries vary substantially in avererage volatility (although the comparison is not perfectly accurate because the panel is unbalanced)
s     = unique(avgvol)
plot(s)

# WLS using lm() with argument 'weights'. Here weight[i] is set to the inverse (estimated) variance for that country
weights = 1/(avgvol^2)
lm.fit = lm(excessret~logCAPE+momentum+vol3m, weights = weights)

b_wls = coef(lm.fit)            

# let's compare b_ols these 3 estimators b_wls
print(b_ols)
print(b_wls)

# Note: We would obtain the same result by multiplying y and x by 1/avgvol = sqrt(weight), where x should include a constant (a column of ones)



