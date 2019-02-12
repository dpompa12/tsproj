library(readxl)
antemp = read_excel("C:/Users/84pom/Downloads/antemp.xlsx")
plot(x=antemp$Year, y=antemp$Glob, xlab="Year", ylab="Global Temperature Deviations", main = "Global Annual Temperature Deviations from Corresponding 1951-1980 Means", type = 'o')
abline(h=0, col=2)
temp = ts(antemp$Glob, start=1880, end=2017, frequency=1)
tempdiff = diff(temp)
ts.plot(tempdiff, ylab='Differenced Temperature Deviations')
abline(h=0)
acf(tempdiff)
pacf(tempdiff)
eacf(tempdiff)
#It appears that an ARIMA(1,1,1), ARIMA(0,1,2), or possibly an ARIMA(3,1,0) would be appropriate

model1 = arima(temp, order = c(1,1,1))
model1
model2 = arima(temp, order = c(3,1,0))
model2
model3 = arima(temp, order = c(0,1,2), method = 'ML') #IMA(1,2) model, note that we cannot use MOM estimator so I will force ML method
model3
alpha = 0.05
c(-0.3408 - qnorm(1-alpha/2)*0.0797, -0.3408 + qnorm(1-alpha/2)*0.0797)
#reject null that theta_1 = 0, conclude it is significantly less than 0

c(-0.2108 - qnorm(1-alpha/2)*0.0743, -0.2108 + qnorm(1-alpha/2)*0.0743)
#reject null that theta_2 = 0, conclude it is significantly less than 0



plot(rstandard(model3), ylab="Standardize Residuals")
abline(h=0, col = 2)
qqnorm(rstandard(model3))
qqline(rstandard(model3))
shapiro.test(rstandard(model3))
acf(rstandard(model3))
Box.test(rstandard(model3), type = "Ljung", lag = 2)
runs(rstandard(model3))
#very large p-value for runs test, conclude standardized residuals are independent

#Overfit with IMA(1,3) to further test our proposed model
model4 = arima(temp, order = c(0,1,3))
model4
#This model is worse by AIC

#Test if ma3 coefficient is significantly different than 0:
c(-0.0348 - qnorm(1-alpha/2)*0.0843, -0.0348 + qnorm(1-alpha/2)*0.0843)
#0 is included in the confidence interval so we fail to reject null that ma3=0 and conclude this coefficient is not significant
#conclude that this model is a case of overfitting, and our simpler model is sufficient

#model 3 appears to be best fit but there is issue with acf
#write out equation of specified model:
#Yt- Yt-1 = et -0.3408et-1 -0.2108et-2


#testing dataset
testing = temp[1:132]
test = sarima.for(testing,6,0,1,2)
pred = as.vector(test$pred)
check = temp[133:138]
plot(check, col=2, ylab = "Temperature Deviation", xlab="Lag")
points(pred, col=4)
#We see that up to lag 2, the residual error between the true data and our prediction is very close, but become large for lags after 2 
#Model appears to underestimate the temperature deviation


#prediction of annual temperature deviation from 1951-1980 means up to 2050
sarima.for(temp,33,0,1,2)
