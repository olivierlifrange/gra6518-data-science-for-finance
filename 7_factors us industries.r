#
# Principal components and principal component regressions
#

# Import data, monthly returs of 30 us industries, from 1945

data = read.csv("Data/French30_Industry_Portfolios_Monthly_VW.csv");


dataGE = read.csv("C:/Users/a1810185/Documents/A_Data Science/Data/GlobalEquityReturns.csv", header = TRUE, sep = ",")
colnames(dataGE) = c('date','countrynumber','excessret','logCAPE','momentum','avgvol','vol3m','vol12m')

attach(dataGE, warn.conflicts = F)             # The database 'data' (dataframe or list) is attached to the R search path. Now we can use the variable names.
