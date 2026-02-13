
# 
# Predicting international stock market returs.
#
# The file GlobaEquityReturns.csv has 13565 observations of monthly excess log returns in 36 countries (unbalanced panel, 1926-2020,
# but 1970s-2020 in most countries), and 
# log(CAPE), momentum (cumulative returns in the last 12 months) and various measures of volatility (average, last 3 months, last 12 months). 
# These are arguably the three most powerful "factors" to attempt to predict returns (valuation, momentum, volatility)
#
# Paolo Giordani, September 2020.  


#setwd('C:/Users/a1810185/Documents/A_DataScience/R code')            # if needed, set directory for output     

# date, countrynumber,excessret,logCAPE,momentum,vol3m,vol12m
dataGE = read.csv("./data/GlobalEquityReturns.csv", header = TRUE, sep = ",")

# let's take a look at the data. This shows a problem with vol12m that does not show when visually inspecting the csv file. 
print(summary(dataGE))

colnames(dataGE) = c('date','countrynumber','excessret','logCAPE','momentum','avgvol','vol3m','vol12m')

# Let's see if we can "predict" excess returns with a linear model. 

# you can attach the dataset, or specify it. In both cases, we can then use the variable names in lm.fit.
# Here we specify it. lm( ) -- for linear model -- carries out regression. 
#lm.fit = lm(excessret~logCAPE+momentum+vol3m, data = dataGE)

# ... and here we attach it
attach(dataGE, warn.conflicts = F)             # The database 'data' (dataframe or list) is attached to the R search path. Now we can use the variable names.
lm.fit = lm(excessret~logCAPE+momentum+avgvol+vol3m)

# let's look at the results. avgvol and vol3m have opposite signs, with the second larger.
print(summary(lm.fit))           # display summary of fit.
b     = coef(lm.fit)             # extract coefficients. We could also use lm.fit$coefficients

# let's repeat, this time without avgvol since it doesn't seem to matter
lm.fit2 = lm(excessret~logCAPE+momentum+vol3m)
print(summary(lm.fit2))           # display summary of fit.

# watch out, the t-stat are overstatted because R fails to recognize data is substantially corrolated. (CAN stock market move with US stock market)


