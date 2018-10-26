#Calculating NYSE Pharma Index between "January 1st 2009 - July 1st 2010"
library(lubridate)
####0. Initial Data stuff
### 0.1 Let's read in our data
nysePharma=read.csv("./Risk Management Project/Data/MarketCap/NysePharmaMarketCap.csv", header=T)
head(nysePharma)
colnames(nysePharma)
str(nysePharma)


### 0.2 Let's make a quick check on our missing values in each column
temp=rep(0,18)
for (i in 1:dim(nysePharma)[2])
{
  temp[i]=sum(is.na(nysePharma[,i]))
}
temp

# We see that the NVO.US.Equity(index=13) has 8 missing values at the end.
# Therefore, I am removing it from my table.
nysePharma=nysePharma[,-13]



### 0.3 
#Converting Date Column into 'Date' type object 
nysePharma$Date=as.character(nysePharma$Date)
nysePharma$Date=mdy(nysePharma$Date)





1050:2265
#### 1. First window-> 2009-01-01 to 2013-05-31
### 1.1 Subsetting the range of dates we want
#Subsetting NysePharma in the date range I want
d1= as.Date("2009-01-01")
d2= as.Date("2013-06-01")
myTable=nysePharma[c(1050:2265),]
myTable= subset(nysePharma, nysePharma[,1] >= d1 & nysePharma[,1]<d2)
dim(myTable)

#Taking the date column seperately
d=myTable[,1]
class(d)


#Removing the Date Column from our subsetted dataframe "myTable"
myTable=myTable[,-1]



###1.2 Now, we will calculate the value of the index for each of these dates
#IndexCalculation
indexVal=0

for (i in 1:dim(myTable)[1])
{
  currRow=myTable[i,]
  currRow=as.numeric(currRow)
  weightVector=currRow/sum(currRow)
  currRow=currRow*weightVector
  
  sumcurrRow=sum(currRow)
  indexVal[i]=sumcurrRow
}


### 1.3 We have our required data now. Let's model for GARCH volatility
#Let's start modelling for GARCH
library(fGarch)
library(rugarch)

returns=diff(log(indexVal))
class(returns)
length(returns)
ts.plot(returns)


# Since return calc starts from the second day, we remove the first date value to make the 
# 'indexVal' and 'returns' of equal length
d=d[-1]
length(d)

#Plotting Dates and Returns
plot(d,returns, type="l")


acf(returns)
pacf(returns)
Box.test(returns, lag=10, type="Ljung-Box")

#Applying a ARIMA model on the returns first, then proceeding with the residuals
m1=auto.arima(returns)
m1
res=m1$residuals
acf(res^2)
pacf(res^2)


#Checking for any serial correlations

returnsq=res^2

par(mfcol=c(1,1))
acf(returnsq)
pacf(returnsq)

# Going directly with the returns
returnsq=returns^2
acf(returnsq)
pacf(returnsq)


m2.spec <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution.model = "norm")
m2 <- ugarchfit(data = returnsq, spec = m2.spec)
m2


m2= garchFit(~garch(1,1), data=returnsq, trace=F)
m2
omega= 0
alpha=0.050003
beta=0.900001 


vol1=0
#My window period
n=60
#Initial value of variance
S=0

for (i in 1:length(returnsq)){    
  if (i<=n) {	  
    S = omega+(S*beta) + (returnsq[i]*(alpha))
    #print(S)
  }   else if (i>n){
    S = omega+ (S*beta) + (returnsq[i]*(alpha))
    SD = sqrt(S)
    vol1[i] = SD
    #print(S)
  }
}


#Plotting my value of volatility against date:
length(vol1)
length(d)
d=d[-c(1:60)]
vol1=vol1[-c(1:60)]

plot(d, vol1, type="l", xlab="Date", ylab="Volatility Estimate", main="NYSE Pharmaceutical Index")
months= seq(min(d), max(d), "month")
axis(1, months, format(months, "%Y\n%b"))


legend("topright",
       inset=.05,
       cex = 0.5,
       c("NYSE Index","NASDAQ Index", "NYSE Pharma Index"),
       horiz=TRUE,
       lty=c(1,1),
       lwd=c(2,2),
       col=c("blue","red","black"),
       bg="grey96")


### 1.4 
##Now that I have the daily volatility estimates, I shall begin the process of checking
##whether volatility changes are significant

##The aim here is to calculate the mean() volatility of our main event month from our
##event timeline, the mean() volatility from the month before, and the mean() volatility
##from the month after.
##Then, I took the percentage change between 'month before'-'current month', 
##and 'current month'-'month after'

##To accomplish this, I've had to write a decent amount of code using substr() to 
##correctly get the proper dates of the month before and month after

##At the end, 'pchangeValues' holds the percentage changes, TWO for each date. 

dat=data.frame(d,vol1)
myDates=c("2009-07","2009-08","2009-11","2010-03","2010-06","2010-07","2010-09"
          ,"2011-01","2011-11","2012-06","2012-08","2012-11")
pchangeValues=c()


for (i in myDates)
{
  mainMonthVolValues=dat$vol1[format(dat$d,"%Y-%m")==i]
  print(i)
  
  if(substr(i,6,7)=="01"){
    prevMonthIndex=12
    prevMonthDate=i
    substr(prevMonthDate,3,4)=as.character(as.numeric(substr(prevMonthDate,3,4))-1)
    substr(prevMonthDate,6,7)=as.character(prevMonthIndex)
    print(prevMonthDate)
    prevMonthVolValues=dat$vol1[format(dat$d,"%Y-%m")==prevMonthDate]
    
    nextMonthIndex=2
    nextMonthIndex=paste("0",as.character(nextMonthIndex),sep="")
    nextMonthDate=i
    substr(nextMonthDate,6,7)=as.character(nextMonthIndex)
    print(nextMonthDate)
    nextMonthVolValues=dat$vol1[format(dat$d,"%Y-%m")==nextMonthDate]
    
    mainMonthMean=mean(mainMonthVolValues)
    prevMonthMean=mean(prevMonthVolValues)
    nextMonthMean=mean(nextMonthVolValues)
    
    pchange1=(mainMonthMean-prevMonthMean)/prevMonthMean
    pchange1=pchange1*100
    pchange2=(nextMonthMean-mainMonthMean)/mainMonthMean
    pchange2=pchange2*100
    
    pchangeValues=c(pchangeValues,pchange1,pchange2)
  }
  
  else if(substr(i,6,7)=="12"){
    prevMonthIndex=11
    prevMonthDate=i
    substr(prevMonthDate,6,7)=as.character(prevMonthIndex)
    print(prevMonthDate)
    prevMonthVolValues=dat$vol1[format(dat$d,"%Y-%m")==prevMonthDate]
    
    nextMonthIndex=1
    nextMonthIndex=paste("0",as.character(nextMonthIndex),sep="")
    nextMonthDate=i
    substr(nextMonthDate,3,4)=as.character(as.numeric(substr(nextMonthDate,3,4))+1)
    substr(nextMonthDate,6,7)=nextMonthIndex
    print(nextMonthDate)
    nextMonthVolValues=dat$vol1[format(dat$d,"%Y-%m")==nextMonthDate]
    
    mainMonthMean=mean(mainMonthVolValues)
    prevMonthMean=mean(prevMonthVolValues)
    nextMonthMean=mean(nextMonthVolValues)
    
    
    pchange1=(mainMonthMean-prevMonthMean)/prevMonthMean
    pchange1=pchange1*100
    pchange2=(nextMonthMean-mainMonthMean)/mainMonthMean
    pchange2=pchange2*100
    
    pchangeValues=c(pchangeValues,pchange1,pchange2)
  }
  else{
    prevMonthIndex=as.numeric(substr(i,6,7))-1
    if(prevMonthIndex<10){
      prevMonthIndex=paste("0",as.character(prevMonthIndex),sep="")
    }
    prevMonthDate=i
    substr(prevMonthDate,6,7)=as.character(prevMonthIndex)
    print(prevMonthDate)
    prevMonthVolValues=dat$vol1[format(dat$d,"%Y-%m")==prevMonthDate]
    
    nextMonthIndex=as.numeric(substr(i,6,7))+1
    if(nextMonthIndex<10){
      nextMonthIndex=paste("0",as.character(nextMonthIndex),sep="")
    }
    nextMonthDate=i
    substr(nextMonthDate,6,7)=as.character(nextMonthIndex)
    print(nextMonthDate)
    nextMonthVolValues=dat$vol1[format(dat$d,"%Y-%m")==nextMonthDate]
    
    mainMonthMean=mean(mainMonthVolValues)
    prevMonthMean=mean(prevMonthVolValues)
    nextMonthMean=mean(nextMonthVolValues)
    
    pchange1=(mainMonthMean-prevMonthMean)/prevMonthMean
    pchange1=pchange1*100
    pchange2=(nextMonthMean-mainMonthMean)/mainMonthMean
    pchange2=pchange2*100
    
    pchangeValues=c(pchangeValues,pchange1,pchange2)
    
  }  
}

#### 2. First window-> 2013-06-01 to 2017-12-31
### 2.1 Subsetting the range of dates we want
### There is an added step here. We need to take 61 values before 2013-06-01, so that
### when we begin volatility estimation, it begins AT 2013-06-01
#Subsetting NysePharma in the date range I want
d1= as.Date("2013-06-01")
d2= as.Date("2017-12-31")
myTable=nysePharma[c(1050:2265),]
myTable= subset(nysePharma, nysePharma[,1] >= d1 & nysePharma[,1]<d2)
dim(myTable)

#Taking the date column seperately
d=myTable[,1]
class(d)


#Removing the Date Column from our subsetted dataframe "myTable"
myTable=myTable[,-1]



###2.2 Now, we will calculate the value of the index for each of these dates
#IndexCalculation
indexVal=0

for (i in 1:dim(myTable)[1])
{
  currRow=myTable[i,]
  currRow=as.numeric(currRow)
  weightVector=currRow/sum(currRow)
  currRow=currRow*weightVector
  
  sumcurrRow=sum(currRow)
  indexVal[i]=sumcurrRow
}


### 2.3 We have our required data now. Let's model for GARCH volatility
#Let's start modelling for GARCH
library(fGarch)
library(rugarch)

returns=diff(log(indexVal))
class(returns)
length(returns)
ts.plot(returns)


# Since return calc starts from the second day, we remove the first date value to make the 
# 'indexVal' and 'returns' of equal length
d=d[-1]
length(d)

#Plotting Dates and Returns
plot(d,returns, type="l")


acf(returns)
pacf(returns)
Box.test(returns, lag=10, type="Ljung-Box")

#Applying a ARIMA model on the returns first, then proceeding with the residuals
m1=auto.arima(returns)
m1
res=m1$residuals
acf(res^2)
pacf(res^2)


#Checking for any serial correlations

returnsq=res^2

par(mfcol=c(1,1))
acf(returnsq)
pacf(returnsq)

# Going directly with the returns
returnsq=returns^2
acf(returnsq)
pacf(returnsq)


m2.spec <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution.model = "norm")
m2 <- ugarchfit(data = returnsq, spec = m2.spec)
m2


m2= garchFit(~garch(1,1), data=returnsq, trace=F)
m2
omega= 0
alpha=0.050024
beta=0.900006 


vol2=0
#My window period
n=60
#Initial value of variance
S=0

for (i in 1:length(returnsq)){    
  if (i<=n) {	  
    S = omega+(S*beta) + (returnsq[i]*(alpha))
    #print(S)
  }   else if (i>n){
    S = omega+ (S*beta) + (returnsq[i]*(alpha))
    SD = sqrt(S)
    vol2[i] = SD
    #print(S)
  }
}


#Plotting my value of volatility against date:
length(vol2)
length(d)
d=d[-c(1:60)]
vol2=vol2[-c(1:60)]

plot(d, vol2, type="l", xlab="Date", ylab="Volatility Estimate", main="NYSE Pharmaceutical Index")
months= seq(min(d), max(d), "month")
axis(1, months, format(months, "%Y\n%b"))




### 2.4 
##Now that I have the daily volatility estimates, I shall begin the process of checking
##whether volatility changes are significant

##The aim here is to calculate the mean() volatility of our main event month from our
##event timeline, the mean() volatility from the month before, and the mean() volatility
##from the month after.
##Then, I took the percentage change between 'month before'-'current month', 
##and 'current month'-'month after'

##To accomplish this, I've had to write a decent amount of code using substr() to 
##correctly get the proper dates of the month before and month after

##At the end, 'pchangeValues' holds the percentage changes, TWO for each date. 

dat2=data.frame(d,vol2)
myDates2=c("2013-07","2013-10","2013-11","2013-12","2014-01","2015-03","2015-06"
           ,"2016-05","2016-11","2017-03","2017-04","2017-05","2017-07","2017-10")
pchangeValues2=c()


for (i in myDates2)
{
  mainMonthVolValues=dat2$vol2[format(dat2$d,"%Y-%m")==i]
  print(i)
  
  if(substr(i,6,7)=="01"){
    prevMonthIndex=12
    prevMonthDate=i
    substr(prevMonthDate,3,4)=as.character(as.numeric(substr(prevMonthDate,3,4))-1)
    substr(prevMonthDate,6,7)=as.character(prevMonthIndex)
    print(prevMonthDate)
    prevMonthVolValues=dat2$vol2[format(dat2$d,"%Y-%m")==prevMonthDate]
    
    nextMonthIndex=2
    nextMonthIndex=paste("0",as.character(nextMonthIndex),sep="")
    nextMonthDate=i
    substr(nextMonthDate,6,7)=as.character(nextMonthIndex)
    print(nextMonthDate)
    nextMonthVolValues=dat2$vol2[format(dat2$d,"%Y-%m")==nextMonthDate]
    
    mainMonthMean=mean(mainMonthVolValues)
    prevMonthMean=mean(prevMonthVolValues)
    nextMonthMean=mean(nextMonthVolValues)
    
    pchange1=(mainMonthMean-prevMonthMean)/prevMonthMean
    pchange1=pchange1*100
    pchange2=(nextMonthMean-mainMonthMean)/mainMonthMean
    pchange2=pchange2*100
    
    pchangeValues2=c(pchangeValues2,pchange1,pchange2)
  }
  
  else if(substr(i,6,7)=="12"){
    prevMonthIndex=11
    prevMonthDate=i
    substr(prevMonthDate,6,7)=as.character(prevMonthIndex)
    print(prevMonthDate)
    prevMonthVolValues=dat2$vol2[format(dat2$d,"%Y-%m")==prevMonthDate]
    
    nextMonthIndex=1
    nextMonthIndex=paste("0",as.character(nextMonthIndex),sep="")
    nextMonthDate=i
    substr(nextMonthDate,3,4)=as.character(as.numeric(substr(nextMonthDate,3,4))+1)
    substr(nextMonthDate,6,7)=nextMonthIndex
    print(nextMonthDate)
    nextMonthVolValues=dat2$vol2[format(dat2$d,"%Y-%m")==nextMonthDate]
    
    mainMonthMean=mean(mainMonthVolValues)
    prevMonthMean=mean(prevMonthVolValues)
    nextMonthMean=mean(nextMonthVolValues)
    
    
    pchange1=(mainMonthMean-prevMonthMean)/prevMonthMean
    pchange1=pchange1*100
    pchange2=(nextMonthMean-mainMonthMean)/mainMonthMean
    pchange2=pchange2*100
    
    pchangeValues2=c(pchangeValues2,pchange1,pchange2)
  }
  else{
    prevMonthIndex=as.numeric(substr(i,6,7))-1
    if(prevMonthIndex<10){
      prevMonthIndex=paste("0",as.character(prevMonthIndex),sep="")
    }
    prevMonthDate=i
    substr(prevMonthDate,6,7)=as.character(prevMonthIndex)
    print(prevMonthDate)
    prevMonthVolValues=dat2$vol2[format(dat2$d,"%Y-%m")==prevMonthDate]
    
    nextMonthIndex=as.numeric(substr(i,6,7))+1
    if(nextMonthIndex<10){
      nextMonthIndex=paste("0",as.character(nextMonthIndex),sep="")
    }
    nextMonthDate=i
    substr(nextMonthDate,6,7)=as.character(nextMonthIndex)
    print(nextMonthDate)
    nextMonthVolValues=dat2$vol2[format(dat2$d,"%Y-%m")==nextMonthDate]
    
    mainMonthMean=mean(mainMonthVolValues)
    prevMonthMean=mean(prevMonthVolValues)
    nextMonthMean=mean(nextMonthVolValues)
    
    pchange1=(mainMonthMean-prevMonthMean)/prevMonthMean
    pchange1=pchange1*100
    pchange2=(nextMonthMean-mainMonthMean)/mainMonthMean
    pchange2=pchange2*100
    
    pchangeValues2=c(pchangeValues2,pchange1,pchange2)
    
  }  
}


#### 3. Final t-test
### Now that we have the percentage values for all the dates, we can calculate 
### the t-value. 

pchangeValuesFin=c(pchangeValues,pchangeValues2)

hist(pchangeValuesFin, nclass=12)

x_mean=mean(pchangeValuesFin)
x_sd=sd(pchangeValuesFin)
tvalue= x_mean/(sqrt((x_sd^2)/length(pchangeValuesFin)))
tvalue
#-1.055333







