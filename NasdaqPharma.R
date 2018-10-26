library(lubridate)


###1. Reading in the data
nasdaqPharma=read.csv("./Risk Management Project/Data/MarketCap/NasdaqPharmaMarketCapNewOrig.csv", header=T)
head(nasdaqPharma)
str(nasdaqPharma)
dim(nasdaqPharma)

###2. Let's check for missing values in each column 
# Quick check for missing values
temp=rep(0,81)
for (i in 1:dim(nasdaqPharma)[2])
{
  temp[i]=sum(is.na(nasdaqPharma[,i]))
}
temp
#After looking at the excel sheet, some of them have missing values in the last few rows
#I'm removing the last 8 rows for each column
nasdaqPharma=nasdaqPharma[-c(2270:2278),]

#After checking for missing values again, there are 3 columns that have a large number
#of missing values. I'm finding the index of these columns
which(temp>0)
#35,43,68

#Removing columns c(35,43,68)
colnames(nasdaqPharma)[c(35,43,68)]  #"GNVC.US.Equity" "INSY.US.Equity" "SCLN.US.Equity
nasdaqPharma=nasdaqPharma[,-c(35,43,68)]

#Converting date column into Date type
nasdaqPharma$Date=as.character(nasdaqPharma$Date)
nasdaqPharma[,1]=mdy(nasdaqPharma[,1])
str(nasdaqPharma)

###Subsetting to the dates we want
##I'm using two windows.
##1) 2009-01-01 to 2013-05-31
##2) 2013-06-01 to 2017-12-31


d1= as.Date("2013-06-01")
d2= as.Date("2017-12-31")

myTable=nasdaqPharma[1050:2265,]
myTable= subset(nasdaqPharma, nasdaqPharma[,1] >= d1 & nasdaqPharma[,1] <=d2)
dim(myTable)




#Taking the Date Column seperately
d=myTable[,1]
class(d)


#Removing the Date Column from our subsetted dataframe "myTable"
myTable=myTable[,-1]
class(myTable)
dim(myTable)
str(myTable)


#IndexCalculation
indexVal=rep(0,dim(myTable)[1])

for (i in 1:dim(myTable)[1])
{
  currRow=as.numeric(myTable[i,])
  weightVector=currRow/sum(currRow)
  currRow=currRow*weightVector
  
  sumcurrRow=sum(currRow)
  indexVal[i]=sumcurrRow
}


length(d)
length(indexVal)

#Let's start modelling for GARCH
library(fGarch)
library(rugarch)
library(forecast)

returns=diff(log(indexVal))
length(returns)
class(returns)
ts.plot(returns)
hist(returns)
qqnorm(returns)
qqline(returns)


hist(diff(indexVal))
mySeq=seq(min(diff(indexVal)), max(diff(indexVal)), length=40)



#Plotting returns against the date to see a clearer picture
#since return calc starts from the second day 
d=d[-1]
length(d)
class(d)


plot(d,returns,type="l")



acf(returns)
pacf(returns)
m1=auto.arima(returns)
m1
res=m1$residuals


#Working with the residuals of our ARMA model
returnsq=res^2
acf(returnsq)
pacf(returnsq)

#Going directly with returns
returnsq=returns^2
acf(returnsq)
pacf(returnsq)


#Ljung-Box Test on Returnsq
Box.test(returnsq,lag=10,type='Ljung')


#Applying the Model

m2.spec <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution.model = "norm")
m2 <- ugarchfit(data = returnsq, spec = m2.spec)
m2


m2= garchFit(~garch(1,1), data=returnsq, trace=F)
m2

omega= 1.141e-07
alpha= 1.496e-01  
beta= 7.437e-01   


volTemp=0
vol1=0
vol2=0

#My initial window period
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

#Removing the NA values, and adjusting the same in date (So we can plot them against each other)
vol1=vol1[-c(1:60)]
d=d[-c(1:60)]
length(vol1)
length(d)

vol2=vol2[-c(1:60)]
d=d[-c(1:60)]
length(vol2)
length(d)

plot(d,vol2, type="l",xaxt="n", xlab="Date", ylab="Volatility Estimate", main="Nasdaq Pharmaceutical Index")
months= seq(min(d), max(d), "month")
axis(1, months, format(months, "%Y\n%b"))

legend("topright",
       inset=.05,
       cex = 0.5,
       c("NASDAQ Index","NYSE Index", "NASDAQ Pharma Index"),
       horiz=TRUE,
       lty=c(1,1),
       lwd=c(2,2),
       col=c("blue","red","black"),
       bg="grey96")


#Zooming In
#June 1 2013 - Sept 30 2015
#Index= 1:588

dz=d[c(1:588)]
vol1z=vol1[c(1:588)]

plot(dz,vol1z, type="l",xaxt="n", xlab="Date", ylab="Volatility Estimate", main="Nasdaq Pharmaceutical Index", ylim=c(0.003,0.036))
monthsz= seq(min(dz), max(dz), "month")
axis(1, monthsz, format(monthsz, "%Y\n%b"))

legend("topleft",
       inset=.05,
       cex = 0.5,
       c("NASDAQ Index","NYSE Index", "NASDAQ Pharma Index"),
       horiz=TRUE,
       lty=c(1,1),
       lwd=c(2,2),
       col=c("blue","red","black"),
       bg="grey96")


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
dat2=data.frame(d,vol2)

#d[format(d, "%Y-%m")=="2009-07"]



myDates=c("2009-07","2009-08","2009-11","2010-03","2010-06","2010-07","2010-09"
          ,"2011-01","2011-11","2012-06","2012-08","2012-11")
myDates2=c("2013-07","2013-10","2013-11","2013-12","2014-01","2015-03","2015-06"
           ,"2016-05","2016-11","2017-03","2017-04","2017-05","2017-07","2017-10")
           

#pchangeValues=c()
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
    
    pchangeValues=c(pchangeValues,pchange1,pchange2)
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
    
    pchangeValues=c(pchangeValues,pchange1,pchange2)

  }  
}

hist(abs(pchangeValues), nclass=12)

x_mean=mean(pchangeValues)
x_sd=sd(pchangeValues)
tvalue= x_mean/(sqrt((x_sd^2)/length(pchangeValues)))

#tvalue wasn't significant
# I forgot to note it down. Repeat the process again to find the t-value




# October 1 2015 - December 31 2017
#Index= 589:1155
dz=d[c(589:1155)]
vol1z=vol1[c(589:1155)]

plot(dz,vol1z, type="l",xaxt="n", xlab="Date", ylab="Volatility Estimate", main="Nasdaq Pharmaceutical Index", ylim=c(0.003,0.065))
monthsz= seq(min(dz), max(dz), "month")
axis(1, monthsz, format(monthsz, "%Y\n%b"))



