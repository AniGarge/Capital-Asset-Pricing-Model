# CASE 1 (Capital Asset Pricing Model)
# Aniruddh Garge
# SUID - 863559754


### Loading the necessary libraries
library(e1071)
library(tseries)
library(nortest)


### Read the dataset
CAPM<-read.csv("Case1CAPM.csv", header = TRUE, sep=",")

### General information of the dataset
dim(CAPM)
names(CAPM)
View(CAPM)


### Change the class of date column from integer to Date (Format is YYYY-MM-DD)
class(CAPM$DATE)
DATE<-as.Date(as.character(CAPM$DATE), "%Y%m%d")

### Calculate excess returns of IBM
ibmRET<-CAPM$IBMRET
marketEXERT<-CAPM$MarketEXRET
RF<-CAPM$RF
IBMEXERT<-ibmRET-RF


### Calculate yearly excess returns for market and IBM 
lg<-length(ibmRET)
IBMEXERT_Annualized<-rep(NA,lg)
marketEXERT_Annualized<-rep(NA,lg)
for (i in 252:lg){
  IBMEXERT_Annualized[i]<-(prod(IBMEXERT[(i-252+1):(i)]/100+1)-1)*100
  marketEXERT_Annualized[i]<-(prod(marketEXERT[(i-252+1):(i)]/100+1)-1)*100
}
### 252 because we don't have the previous years records.

### Create a time series plot of yearly returns from the yearly returns calculated above
plot(DATE[252:lg], marketEXERT_Annualized[252:lg], type = "l", col="blue", xlab="Years", ylab="Market Excess Returns",main="Daily Market Excess Return (annualized percentage)", ylim=c(-60, 160))
dev.off()


### find the maximum value 
maximum<-max(marketEXERT_Annualized,na.rm=T)
maxvalue<-grepl(maximum, marketEXERT_Annualized)
findmax<-which(maxvalue) #### which finds the index
DATE[findmax]
marketEXERT_Annualized[findmax]


### Calculate the 5 year annualized return 
IBMEXERT_5Year<-rep(NA,lg)
marketEXERT_5Year<-rep(NA,lg)
for (i in (252*5):lg){
  IBMEXERT_5Year[i]<-(prod(IBMEXERT[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100
  marketEXERT_5Year[i]<-(prod(marketEXERT[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100
}


### Plot the 5 years excess returns with respect to the date
plot(DATE[(252*5):lg], marketEXERT_5Year[(252*5):lg], type = "l", col="blue",xlab="Date", ylab="5 Year Market Annualized Excess Returns", main="Daily Market Excess returns (annualized percentage)",ylim=c(-10, 60))
plot(DATE[(252*5):lg], IBMEXERT_5Year[(252*5):lg], type = "l", col="red",xlab="Date", ylab="5 Year Market Annualized IBM Returns", main="Daily IBM Excess returns (annualized percentage)",ylim=c(-10, 60))


### Mean for different returns
mean(IBMEXERT_Annualized[252:lg]) 
mean(IBMEXERT_5Year[(252*5):lg]) 
mean(marketEXERT_Annualized[252:lg])
mean(marketEXERT_5Year[(252*5):lg]) 


### Box plots for Excess Returns
boxplot(x = CAPM$MarketEXRET,main='Boxplot for Market Excess Returns',ylab='Returns')
boxplot(x = CAPM$IBMRET,main='Boxplot for IBM Excess Returns',ylab='Returns')


### Scatter plot for IBM vs Market Excess Returns
plot(CAPM$MarketEXRET,CAPM$IBMRET,main='Scatter Plot for IBM vs Market Excess Returns',col='Red',xlab='Market Excess Returns',ylab='IBM Returns')


### Numerical statistics for Market Returns
MKTmean<-mean(marketEXERT)*252
MKTsd<-sd(marketEXERT)*sqrt(252)
MKTskew<-skewness(marketEXERT)
MKTkurto<-kurtosis(marketEXERT)
MKTmin<-min(marketEXERT)
MKTmax<-max(marketEXERT)
MKTsr<-MKTmean/MKTsd               #### Sharpe Ratio

MKTVaR<-quantile(marketEXERT, probs = c(0.05))

#### Shortfall 
numES<-lg*0.05
numESInteger<-floor(numES)
numESDecimal<-numES-numESInteger
datasort<-sort(marketEXERT, decreasing = FALSE)
MKTES<-sum(datasort[1:numESInteger]+datasort[numESInteger+1]*numESDecimal)/numES
IBMcMarket<-cor(IBMEXERT, marketEXERT)                   #### Correlation


IBMmean_Daily<-mean(IBMEXERT)*252
IBMsd_Daily<-sd(IBMEXERT)*sqrt(252)
IBMskew_Daily<-skewness(IBMEXERT)
IBMkurto_Daily<-kurtosis(IBMEXERT)
IBMmin_Daily<-min(IBMEXERT)
IBMmax_Daily<-max(IBMEXERT)
IBMsr_Daily<-IBMmean_Daily/IBMsd_Daily               #### Sharpe Ratio
IBMVaR_Daily<-quantile(IBMEXERT, probs = c(0.05))

#### Shortfall 
numES<-lg*0.05
numESInteger<-floor(numES)
numESDecimal<-numES-numESInteger
datasort<-sort(IBMEXERT, decreasing = FALSE)
IBMES<-sum(datasort[1:numESInteger]+datasort[numESInteger+1]*numESDecimal)/numES


### Generate a dataframe for the overall descriptive statistics
Name<-c("Mean:", "Std:", "Skewness:", "Kurtosis:",
        "Sharpe Ratio","Value at Risk","Expected Shortfall","Correlation:" )
IBM<-c(IBMmean_Daily, IBMsd_Daily, IBMskew_Daily, IBMkurto_Daily, IBMsr_Daily, IBMVaR_Daily, IBMES, IBMcMarket)
Market<-c(MKTmean, MKTsd, MKTskew, MKTkurto, MKTsr,MKTVaR, MKTES, NA)

data.frame(round(IBM,4), round(Market,4),row.names =Name,check.names = TRUE)


### Plot the histograms 
hist(IBMEXERT,prob= T,freq = F,breaks=50,xlab = 'IBM Daily Returns', ylab = 'Density',main = 'Histogram of IBM using density',ylim = c(0,0.35))
curve(dnorm(x,mean(IBMEXERT),sd(IBMEXERT)),add = T)

hist(marketEXERT,prob=T,freq = F,breaks=50,xlab = 'Market Daily Returns', ylab = 'Density',main = 'Histogram of Market using density',ylim = c(0,0.50))
curve(dnorm(x,mean(marketEXERT),sd(marketEXERT)),add = T)


### QQ Plot for the variables
#### QQ plot tells how near or far are we from a normal distribution (mean=0,sd=1), on X axis we have theoretical quantiles
#### and on Y axis we have quantiles from the data.

qqnorm(marketEXERT, main="Q-Q plot of Market returns")
qqline(marketEXERT)

qqnorm(IBMEXERT, main="Q-Q plot of IBM returns")
qqline(IBMEXERT)



### The Jarque Bera Test
#### This test looks at the skewness and the kurtosis to find for normal distribution, it is reliable
#### for large datasets
jarque.bera.test(IBMEXERT)
jarque.bera.test(marketEXERT)


### The Lilliefors Test
#### It is used when we do not know population mean and standard deviation. The null hypothesis is the data comes from the normal distribution
lillie.test(IBMEXERT)
lillie.test(marketEXERT)


### Model Estimate
Model<-lm(IBMEXERT~marketEXERT)
#### IBM Returns is the dependant variable and Market returns is independant variable.
summary(Model)

Model$coefficients
names(summary(Model))
summary(Model)[["coefficients"]] #### Coefficient for the independant variable and the intercept with the t-values and p-values.
summary(Model)[["call"]]
summary(Model)[["sigma"]]
summary(Model)[["df"]]
summary(Model)[["r.squared"]] #### R squared explains how much variance is explained by the model.
summary(Model)[["fstatistic"]]#### F statistic shows if there is any relationship betweeen predictor and response variable. The greater the better.
#### We define a linear model to find a relationship between market returns and IBM returns.

plot(marketEXERT, IBMEXERT,main="Scatter Plot of IBM Excess returns Vs. Market Excess returns",xlab= "Market Excess returns", ylab="IBM Excess returns")
abline(lm(IBMEXERT~marketEXERT), col="blue")
#### This is the line that fits the best to the data.

testValue<-0
Model<-lm(IBMEXERT~marketEXERT)
estimatedcoeff<-Model$coefficients[1]
estimatedstd<-summary(Model)[["coefficients"]][1,2]
tstats<-(estimatedcoeff-testValue)/estimatedstd
decisionRule<-abs(tstats)>qt(0.975, length(marketEXERT)-1-1)
Result<-ifelse(decisionRule, "Reject", "Can't Reject")
Result
#### Above process is used to reject or accept null hypothesis.

testValue<-1
Model<-lm(IBMEXERT~marketEXERT)
estimatedcoeff<-Model$coefficients[2]
estimatedstd<-summary(Model)[["coefficients"]][2,2]
tstats<-(estimatedcoeff-testValue)/estimatedstd
decisionRule<-tstats>qt(0.95, length(marketEXERT)-1-1)
Result<-ifelse(decisionRule, "Reject", "Can't Reject")
Result
