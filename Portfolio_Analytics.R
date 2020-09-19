#####################################   RISK AND RETURNS oN STOCKS  ##################################################
######################################  19MBMB21,GARIMA GUPTA ##################################################
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
# Installing and Loading the required library

#install.packages("corrplot")
#install.packages("PerformanceAnalytics")
#install.packages("tseries")
#install.packages("zoo")
# Corrplot library is for vizualization of correlation metrics of differen assets in portfolio
library(corrplot)
# PerformanceAnalytics is library of econometric functions for performance and risk analysis of financial instruments or portfolios
library(PerformanceAnalytics)
# Here we are dealing with TimeSeries data for different assets which is dealt with tseries library
library(tseries)
# zoo library is used for creating zoo object that is dealing with timeseries data in an index attribute
library(zoo)

-----------------------------------------------------------------------------------------------------------

# Extracting Adjusted Closing price for TCS compressed on 'daily' using historical financial data from yahoo finance
# We are considering companies registered in National Stock Exchange like here 'TCS.NS'
tcsDailyPrices <- get.hist.quote(instrument = "TCS.NS", start = "2004-07-01",
                                 end = "2018-06-01", quote = "AdjClose",
                                 provider = "yahoo", origin = '1970-01-01',
                                 compression = "d", retclass = "zoo")
# We have taken the Adjusted closing price for computing true value of stock within specific trading days,stored in zoo object.
start(tcsDailyPrices) # Checking for starting date from when it registered
end(tcsDailyPrices)   # Checking for ending date
# Removing NA values for further execution
tcsDailyPrices <- na.omit(tcsDailyPrices)
# Checking for number of observation extracted from source
nrow(tcsDailyPrices)
# total observation= 3392
---------------------------------------------------------------------

#Extracting Adjusted Closing price Infosys stock compressed on 'daily' using historical financial data from yahoo finance
infyDailyPrices <- get.hist.quote(instrument = "INFY.NS", start = "2004-07-01",
                                  end = "2018-06-01", quote = "AdjClose",
                                  provider = "yahoo", origin = '1970-01-01',
                                  compression = "d", retclass = "zoo")
start(infyDailyPrices) #Checking for starting date 
end(infyDailyPrices)   # Checking for ending date
# Removing NA values for further execution
infyDailyPrices <- na.omit(infyDailyPrices)
# Checking for number of observation extracted from source
nrow(infyDailyPrices)
# Total observation = 3430
---------------------------------------------------------------------

#Extracting Adjusted Closing price for Reliance stock compressed on 'daily' using historical financial data from yahoo finance
relDailyPrices <- get.hist.quote(instrument = "RELIANCE.NS", start = "2004-07-01",
                                 end = "2018-06-01", quote = "AdjClose",
                                 provider = "yahoo", origin = '1970-01-01',
                                 compression = "d", retclass = "zoo")
start(relDailyPrices)  #Checking for starting date 
end(relDailyPrices)   # Checking for ending date
# Removing NA values for further execution
relDailyPrices <- na.omit(relDailyPrices)
# Checking for number of observation extracted from source
nrow(relDailyPrices)
#Total observation = 3431
---------------------------------------------------------------------

# Extracting Stock Prices for S&P 500 Index compressed on 'daily' basis using its ticker symbol   
sp = get.hist.quote(instrument = "^gspc",start = "2004-07-01",
                    end = "2018-06-01",quote = "AdjClose",provider = "yahoo",
                    origin = "1970-01-01",
                    compression = "d", retclass = "zoo")
# Removing NA values 
sp = na.omit(sp)
# Checking for number of observation extracted from source
nrow(sp) # 3504 rows

---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

# Extracting Adjusted Closing price for TCS compressed on 'monthly' using historical financial data from yahoo finance
tcsMonthlyPrices <- get.hist.quote(instrument = "TCS.NS", start = "2004-07-01",
                                   end = "2018-06-01", quote = "AdjClose",
                                   provider = "yahoo", origin = '1970-01-01',
                                   compression = "m", retclass = "zoo")
# Removing NA values 
tcsMonthlyPrices <- na.omit(tcsMonthlyPrices)

--------------------------------------------------------------------

#Extracting Adjusted Closing price Infosys stock compressed on 'monthly' using historical financial data from yahoo finance
infyMonthlyPrices <- get.hist.quote(instrument = "INFY.NS", start = "2004-07-01",
                                    end = "2018-06-01", quote = "AdjClose",
                                    provider = "yahoo", origin = '1970-01-01',
                                    compression = "m", retclass = "zoo")
# Removing NA values 
infyMonthlyPrices <- na.omit(infyMonthlyPrices)

--------------------------------------------------------------------

#Extracting Adjusted Closing price for Reliance stock compressed on 'monthly' using historical financial data from yahoo finance
relMonthlyPrices <- get.hist.quote(instrument = "RELIANCE.NS", start = "2004-07-01",
                                   end = "2018-06-01", quote = "AdjClose",
                                   provider = "yahoo", origin = '1970-01-01',
                                   compression = "m", retclass = "zoo")
# Removing NA values 
relMonthlyPrices <- na.omit(relMonthlyPrices)

--------------------------------------------------------------------

# Extracting Adjusted Closing price for S&P 500 Index compressed on 'monthly' basis using its ticker symbol 
# S&P 500 is index stock including top 500 companies working on different sectors of industries,providing diverse portfolio investments.
sp = get.hist.quote(instrument = "^gspc",start = "2004-07-01",
                    end = "2018-06-01",quote = "AdjClose",provider = "yahoo",
                    origin = "1970-01-01",
                    compression = "m", retclass = "zoo")
# Removing NA values 
sp = na.omit(sp)
# Checking for number of observation extracted from source
nrow(sp)# 176 rows number
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------  


# Merging Daily prices for 3 taken stocks and checking for the NULL values in the merged dataset

DailyPrices = merge(tcsDailyPrices, infyDailyPrices,relDailyPrices)
if(nrow(DailyPrices)==
   length(complete.cases(DailyPrices))){
  cat("No null values in the dataset\n")
} else{
  cat("Caution, Null values in the dataset")
}
 # NO NULL VALUES DETECTED
-----------------------------------------------------------------------------------------------------

# Merging Monthly prices for 3 taken stocks and checking for the NULL values in the merged dataset

MonthlyPrices = merge(tcsMonthlyPrices, infyMonthlyPrices, relMonthlyPrices)
if(nrow(MonthlyPrices)==
   length(complete.cases(MonthlyPrices))){
  cat("No null values in the dataset\n")
} else{
  cat("Caution, Null values in the dataset")
}

# NO NULL VALUES DETECTED

----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

#Daily Returns - xts: Time Series - simple method to calculate historical return: [(final - initial) / initial]
tcsDailyReturns <- Return.calculate(xts(tcsDailyPrices),method = "simple")
infyDailyReturns <- Return.calculate(xts(infyDailyPrices),method = "simple")
relDailyReturns <- Return.calculate(xts(relDailyPrices),method = "simple")
# The xts creates extensible time series object with extension to zoo object.

#omiting NA values from daily returns for each stock
tcsDailyReturns <- na.omit(tcsDailyReturns)
infyDailyReturns <- na.omit(infyDailyReturns)
relDailyReturns <- na.omit(relDailyReturns)

#Merging the monthly returns of the 3 assets and checking for NULL Values
DailyReturns <- merge(tcsDailyReturns,infyDailyReturns,relDailyReturns)
if(nrow(DailyReturns)==
   length(complete.cases(DailyReturns))){
  cat("No null values in the dataset\n")
} else{
  cat("Caution, Null values in the dataset")
}

# NO NULL VALUES DETECTED
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

# Using PerformanceAnalytics library computing Returns for stock
# Calculating Monthly returns in zoo object form of each stock using Simple method [(final-initial)/initial] 
tcsMonthlyReturns <- Return.calculate(xts(tcsMonthlyPrices),method = "simple")
infyMonthlyReturns <- Return.calculate(xts(infyMonthlyPrices),method = "simple")
relMonthlyReturns <- Return.calculate(xts(relMonthlyPrices),method = "simple") 

# Omitting NA Values from calculated returns values of each stock
tcsMonthlyReturns <-na.omit(tcsMonthlyReturns)
infyMonthlyReturns <- na.omit(infyMonthlyReturns)
relMonthlyReturns <- na.omit(relMonthlyReturns)

#Merging the calculated monthly returns of the 3 assets and checking for NULL Values at a time
MonthlyReturns <- merge(tcsMonthlyReturns,infyMonthlyReturns,relMonthlyReturns)
if(nrow(MonthlyReturns)==
   length(complete.cases(MonthlyReturns))){
  cat("No null values in the dataset\n")
} else{
  cat("Caution, Null values in the dataset")
}

# NO NULL VALUES DETECTED
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

#Expected Risk is defined as chance for single or combination if assets that do not meet financial objectives
# The expected risk is calculated as the standard deviation of underlying normal distribution(deviation from mean)
cat("Risk associated with TCS: ", round(StdDev(tcsMonthlyReturns),4))
cat("Risk associated with Infosys: ", round(StdDev(infyMonthlyReturns),4))
cat("Risk associated with Reliance: ", round(StdDev(relMonthlyReturns),4))

# Lowest Risk is recorded for TCS(0.0821) and highest risk for Reliance(0.3263)
# We as investor prefer to choose TCS over all 3 assets as lowest risk is desired from stock.
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

#Expected Returns is defined as stock anticipated performance and the overall profit or loss from historical data.
# The expected Returns of stock is calculated using the mean of underlying normal distribution
cat("Expected Return for TCS: ",round(mean(tcsMonthlyReturns, na.rm = TRUE),4))
cat("Expected Return for Infosys: ",round(mean(infyMonthlyReturns, na.rm = TRUE),4))
cat("Expected Return for Reliance: ",round(mean(relMonthlyReturns, na.rm = TRUE),4))

# Lowest Return is recorded for TCS(0.0233) and highest return for Reliance(0.0413)
# If we only consider Expected return we invest in Reliance as with highest Expected return.
# Only on basis of risk or only on basis of high returns we cannot take investement decsions, hence risk-to-reward ratio.
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

# Calculating Coefficient of Variation:
# This is risk-to-reward ratio where volatility od stock is defined,used for investment decisions.
# The investor seeks assets having lower ratio denoting the most optimal risk-to-reward ratio with low volatility but high returns
cat("Coefficient of variation of TCS: ", round(StdDev(tcsMonthlyReturns),4)/round(mean(tcsMonthlyReturns, na.rm = TRUE),4))
cat("Coefficient of variation of Infosys: ", round(StdDev(infyMonthlyReturns),4)/round(mean(infyMonthlyReturns, na.rm = TRUE),4))
cat("Coefficient of variation of Reliance: ", round(StdDev(relMonthlyReturns),4)/round(mean(relMonthlyReturns, na.rm = TRUE),4))

----------------------------------------------------------------------------------------------------------------
# CONCLUSION:
----------------------------------------------------------------------------------------------------------------
# Different individual perceive different values of Returns and risk; hence expected Returns and risk is calculated.
# But, this only not help to compare different assets and conclude which assets to consider for Portfolio.Hence, Coefficient of Variation is to be used.
# Coefficient of variation (CV) is a statistical measure of relative variability indicating risk per unit return = Standard Deviation of Returns/Expected Rate of Returns
# With the help of CV, we can check for optimal tradeoff between Expected return and Risk and a better selecting criterion for choosing best assets for Portfolio.
  
  # The coefficient of variation for various stocks are as follows:

  #Coefficient of variation of TCS:  3.523605
#Coefficient of variation of Infosys:  5.51816
#Coefficient of variation of Reliance:  6.578629 
# LEAST THE COEFFICIENT OF VARIATION - BEST IS THAT ASSET TO INCLUDE IN PORTFOLIO: 3.523605 for TCS
# After calculating this we can choose TCS stock for investing as it will have higher returns with low volatility

---------------------------------------------------------------------------------------------------------------
# Creating Time plots for Monthly prices of assets:
TCSInfrelmonthly_prices = merge(tcsMonthlyPrices,infyMonthlyPrices,relMonthlyPrices)
head(TCSInfrelmonthly_prices)
colnames(TCSInfrelmonthly_prices)= c("TCS","Infosys","Reliance")

par(mfrow=c(2,1))
# Plot showing Monthly prices
plot.zoo(TCSInfrelmonthly_prices,main=" ",lwd=2,col="blue")
par(mfrow=c(1,1))

---------------------------------------------------------------------------------------------------------

my.panel = function(...){
  lines(...)
  abline(h=0)
}

# Plotting Time plot for combinely monthly Returns
TCSInfyRel_monthlyreturns = merge(tcsMonthlyReturns,infyMonthlyReturns,relMonthlyReturns)

# Omitting the NA values
TCSInfyRel_monthlyreturns = na.omit(TCSInfyRel_monthlyreturns)
colnames(TCSInfyRel_monthlyreturns)= c("TCS","Infosys","Reliance")
plot.zoo(TCSInfyRel_monthlyreturns,main=" ",panel= my.panel,lwd=2,col="blue")

-------------------------------------------------------------------------------------------------------------

  # Plotting all 3 returns on same graph with variation of colors
plot.zoo(TCSInfyRel_monthlyreturns,plot.type = "single",main = "",col= c("brown","blue","black"),lty = c("dashed","dotted","solid"),
           lwd=2,ylab = "Returns")
abline(h=0)
legend(x="bottomright",legend = colnames(TCSInfyRel_monthlyreturns),lty = c("dashed","dotted","solid"),lwd=2,col= c("brown","blue","black"))
  
-------------------------------------------------------------------------------------------------------------
  # Creating Time plots for Daily prices of assets:
  TCSInfreldaily_prices = merge(tcsDailyPrices,infyDailyPrices,relDailyPrices)
head(TCSInfreldaily_prices)
colnames(TCSInfreldaily_prices)= c("TCS","Infosys","Reliance")

par(mfrow=c(2,1))
# Plot showing Monthly prices
plot.zoo(TCSInfreldaily_prices,main=" ",lwd=2,col="blue")
par(mfrow=c(1,1))

------------------------------------------------------------------------------------------------------------
  
# Plotting Daily Returns
    my.panel = function(...){
    lines(...)
    abline(h=0)
  }

# Plotting Time plot for combinely monthly Returns
TCSInfyRel_dailyreturns = merge(tcsDailyReturns,infyDailyReturns,relDailyReturns)

# Omitting the NA values
TCSInfyRel_dailyreturns = na.omit(TCSInfyRel_dailyreturns)
colnames(TCSInfyRel_dailyreturns)= c("TCS","Infosys","Reliance")
plot.zoo(TCSInfyRel_dailyreturns,main=" ",panel= my.panel,lwd=2,col="blue")

------------------------------------------------------------------------------------------------------------
  
# Equity curve representing investment amount for each asset growing over time
  
  TCS_eqtycurve = cumprod(1+ tcsMonthlyReturns)
  infy_eqtycurve = cumprod(1+ infyMonthlyReturns)
  Rel_eqtycurve = cumprod(1+ relMonthlyReturns)
  
  TCS_eqtycurve = na.omit(TCS_eqtycurve)
  infy_eqtycurve = na.omit(infy_eqtycurve)
  Rel_eqtycurve = na.omit(Rel_eqtycurve)
  
  dataToplot = merge(TCS_eqtycurve,infy_eqtycurve,Rel_eqtycurve)
  dataToplot = na.omit(dataToplot)
  head(dataToplot)
  colnames(dataToplot) =  c("TCS","Infosys","Reliance")
  plot.zoo(dataToplot,plot.type = "single", main = "",col=c("red","blue","black"),ylab = "Cumulative Returns",
           lwd=2)
  legend(x="topleft",legend = colnames(dataToplot),lwd=2,col= c("red","blue","black"))
   
##############################################################################################################
   
  
  