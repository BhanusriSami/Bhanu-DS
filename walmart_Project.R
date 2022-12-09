install.packages("lubridate")
install.packages(c("raster"))
library(dplyr)
library(lubridate)
library(sp)
library(raster)
library(usdm)
install.packages("lmtest")
library(zoo)
library(lmtest)

W_DF=read.csv("Walmart_Store_sales.csv", header=TRUE)

View(W_DF)

summary(W_DF)

## Finding the Store which has Maximum sales

tmp_mxsal=aggregate(Weekly_Sales~Store,data=W_DF,sum)
tmp_mxsal=arrange(tmp_mxsal,-Weekly_Sales)
tmp_mxsal[1,]

## Finding the store which has Maximum Standard Deviation 

tmp_mxsd=aggregate(Weekly_Sales~Store,data=W_DF,sd)
tmp_mxsd=arrange(tmp_mxsd,-Weekly_Sales)
tmp_mxsd[1,]


## Finding Stores which has Growth in Q3

W_DF$Date=as.Date(W_DF$Date,format="%d-%m-%Y")
W_DF=cbind(W_DF,QtrYr=paste("Q",quarter(W_DF$Date),year(W_DF$Date),sep=""))
TMP=filter(W_DF,QtrYr == "Q22012" | QtrYr == "Q32012")
TMP_WKLY=aggregate(Weekly_Sales~Store+QtrYr,data=TMP,sum)
View(TMP_WKLY)
Q2_sales=filter(TMP_WKLY,QtrYr=="Q22012") 
Q2_sales=Q2_sales[-2]
colnames(Q2_sales)[2] = "Q2_Sales"
View(Q2_sales)
Q3_sales=filter(TMP_WKLY,QtrYr=="Q32012") 
Q3_sales=Q3_sales[-2]
colnames(Q3_sales)[2] = "Q3_Sales"
sales=merge(Q2_sales,Q3_sales,by="Store")
sales=mutate(sales,growth=(Q3_Sales-Q2_Sales)/Q2_Sales)
View(sales)
sales$growth=sales$growth>0
filter(sales,growth==TRUE)

### Finding out the Holidays which has more sales than non-holiday 

summary(W_DF)


NO_HSale=mean(filter(W_DF,Holiday_Flag == 0)$Weekly_Sales)
NO_HSale
TMP=filter(W_DF,Holiday_Flag == 1)
rm(HSale)
HSale=aggregate(Weekly_Sales~Date,data=TMP,mean)
HSale$More=HSale$Weekly_Sales>NO_HSale
HSale=arrange(HSale,-More)
filter(HSale,HSale$More==TRUE)

#### Adding Date Fields to the Output 


W_DF$Date=as.Date(W_DF$Date,format="%Y-%m-%d")
W_DF$Qtr = quarter(W_DF$Date)
colnames(W_DF)
W_DF$Month= month(W_DF$Date)
W_DF$Weekday=weekdays(W_DF$Date)
W_DF$day=day(W_DF$Date)
aggregate(Weekly_Sales~Qtr,data=W_DF,sum)
aggregate(Weekly_Sales~Month,data=W_DF,sum)
View(W_DF)


## to find the count in each group 
summarize(group_by(W_DF,Store),n())


View(S1)
plot(S1)
cor(S1)

#multicollinearity/vif is checked for ind var

vifstep(S1[-1])

###Method1--- VIF  

vifm1=lm(Weekly_Sales~Unemployment+Fuel_Price+CPI,data=S1)
summary(vifm1)

### Method2--- Significance 

sig1=lm(Weekly_Sales~.,data=S1)
summary(sig1)

sig2=lm(Weekly_Sales~.-Fuel_Price,data=S1)
summary(sig2)

sig3=lm(Weekly_Sales~CPI,data=S1)
summary(sig3)

### Method 3--- AIC 


step1=step(sig1)
summary(step1)

## Error Assumptions for the residuals are checked for both the models sig2,vifm1

dwtest(sig2)
plot(sig2)

dwtest(vifm1)
plot(vifm1)

##step1$fitted.values
##step1$residuals[1:5]
##step1$fitted.values[1:5]
##S1$Weekly_Sales[1:5]
####identical(step1$residuals,S1$Weekly_Sales-step1$fitted.values)

dev.off()

##MAPE values are calculated for models-sig2,vifm1

mean(abs(sig2$residuals/S1$Weekly_Sales))
mean(abs(vifm1$residuals/S1$Weekly_Sales))

### Predicted values are calculated for store2 using sig2 model

S2=subset(W_DF,Store==2,select=c("Weekly_Sales","Fuel_Price","CPI","Unemployment"))
plot(S2)
cor(S2)
DS2=predict(sig2,newdata=S2)
DS2
