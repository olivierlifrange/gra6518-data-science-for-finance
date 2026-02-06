
# 
# Predicting international stock market returs: local regression. 
#
# See OLS.r for a description of the data and of some of the functions
#
# Note: The R package locfit performs local regression and local likelihood, but 
# we'll be using our own simpler functions here. 
#
# Paolo Giordani, September 2020.  


# user's defined inputs 

s     = 0.3     # span. k = s*n, k being the number of neighbours  

# end user's define inputs 


source('LocalReg_functions.r')   


dataGE = read.csv("C:/Users/a1810185/Documents/A_Data Science/Data/GlobalEquityReturns.csv", header = TRUE, sep = ",")
colnames(dataGE) = c('date','countrynumber','excessret','logCAPE','momentum','avgvol','vol3m','vol12m')

# OLS with 3 predictors
attach(dataGE, warn.conflicts = F)             # The database 'data' (dataframe or list) is attached to the R search path. Now we can use the variable names.

# ols coefficients 
lm.fit = lm(excessret~logCAPE+momentum)
print(summary(lm.fit))          
b_ols = coef(lm.fit)            

# local regression
y    = dataGE$excessret
xk   = dataGE$logCAPE 
#xreg = dataGE$logCAPE
xreg = cbind(dataGE$logCAPE,dataGE$momentum)


# local regression, from high valuation
xkf   = mean(xk) + 2*sd(xk)        # high valuations
xregf = xreg[1,]                   # not relevant for our purpuses here (we are not forecasting, just comparing ols estimates) 
K     = round(NROW(y)*s)

lr = locregPG(2,K,y,xk,xreg,xkf,xregf) 

b_lr = lr$b

print(" b OLS")
print(b_ols)
print(" b Local Reg. from high valuations")
print(b_lr)


# local regression, from low valuation
xkf   = mean(xk) - 2*sd(xk)        # high valuations
xregf = xreg[1,]                   # not relevant for our purpuses here (we are not forecasting, just comparing ols estimates) 
K     = round(NROW(y)*s)

lr = locregPG(2,K,y,xk,xreg,xkf,xregf) 

b_lr = lr$b

print(" b OLS")
print(b_ols)
print(" b Local Reg. from low valuations")
print(b_lr)


