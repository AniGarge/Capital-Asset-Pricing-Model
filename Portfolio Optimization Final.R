library(PerformanceAnalytics)
library(fPortfolio)

head(myxts)

tail(myxts)

# fPortfolio package prefers time series objects.

as.timeSeries(myxts)-> mytimeSeries

str(mytimeSeries)

head(mytimeSeries)

# Time series is same as xts objects, only difference is GMT(as heading for dates) present in time series object

minvariancePortfolio(mytimeSeries)-> minrisk

minrisk

# Cov is not the covariance, it is the combined volatility (standard deviation) 2.75%

# Max Sharpe Ratio means we take more risk for additional rewards and returns.

# R uses decimal numbers instead of % for return calculation.

portfolioSpec()-> rf

# rf saves portfolio specifications

0.0017-> setRiskFreeRate(rf)
rf

tangencyPortfolio(mytimeSeries, rf)-> maxSharpe

# We use tangency portfolio as it maximize the sharpe ratio

maxSharpe

maxsharpeRatio=(0.0137-0.0017)/0.0327 # Std dev
             #mean  #risk free rate

portfolioFrontier(mytimeSeries,rf)-> myfrontier
plot(myfrontier)

# It is the front border line of all possible portfolios using the stocks given.

# The black points indicate the efficient frontier, as with the same risk you can get higher returns. 

# Export the data from frontier to find out weights of each stock.

frontierPoints(myfrontier)-> mypoints

tail(mypoints)

# Frontier is built by 50 points by default.

getWeights(myfrontier)-> myweights

tail(myweights)

cbind(mypoints,myweights)-> myexport

write.csv(myexport,file="Optimized Portolio.csv")












