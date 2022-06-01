#Q1


AR2.sim <- function(n, ar1, ar2, sd)
{
  Xt = c(0,0)
  for (i in 2:n)
  {
    Xt[i+1] = ar1*Xt[i] + ar2*Xt[i-1] + rnorm(1,mean=0,sd=sd)
  }
  Xt
}
Xt = AR2.sim(100, 0, 0, 0.5)
plot(Xt, type = "o",xlab ="Time" ,ylab =expression("Z"[t]) ,main = expression(paste("AR(2) plot with, ", Phi[1], "=0, ", Phi[2],
                                                                                    "=0")))
acf.results<-acf(Xt, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("Plot of ACF of AR(2)  with, ", Phi[1], "=0, ", Phi[2],
                                        "=0")))

#Q2

Pt <- function(n, ar1, sd)
{
  Xt = c(0,0)
  for (i in 2:n)
  {
    Xt[i+1] = ar1*Xt[i] + rnorm(1,mean=0,sd=sd)
  }
  Xt
}
Yt = Pt(100, 0.8, 1)

Zt=c(0,0)
for (i in 1:100)
{
  Zt[i]=Yt[i]+ rnorm(1,mean=0,sd=1)
}

plot(Zt, type = "o",xlab ="Time" ,ylab =expression("Z"[t]) ,main = expression(paste("Plot of Z "[t], "= Y, "[t],"+,",
                                                                                    mu[t])))
acf.results<-acf(Zt, lag.max = 20, type = "partial",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("Partial ACF Plot of Z "[t], "= Y, "[t],"+,",
                                        mu[t])))
#since the given ts is basically an auto regressive model, and since it has
#highest autocorrelation at lag=2 we set p=2, and q=0

AR <- arima(Zt, order = c(2,0,0))
print(AR)

ts.plot(Zt,xlab="Time", ylab =expression("Z"[t]),main = expression(paste("Plot of Z "[t], "= Y, "[t],"+,",
                                                                         mu[t])))
AR_fit <- Zt - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)



#Q3
library(readr)
Average_population <- read_csv("C:/4th_sem/CFM/For MSc, MTech/For MSc, MTech/Average_population_age.csv")

time_series<-ts(Average_population$`Average age of  the population by sex`)
plot(time_series,type='o',ylab="Average age", main='Average age of  the population by sex')


dif1<-diff(time_series,differences = 1)
plot(dif1,type='o',ylab='Yt',main="Plot of 1st difference")

acf_1st<-acf(dif1, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf_1st,main= expression(paste("ACF plot for 1st difference")))


dif2<-diff(dif1,differences = 1)
plot(dif1,type='o',ylab='Yt',main="Plot of 2nd difference")

acf_2nd<-acf(dif2, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf_2nd,main= expression(paste("ACF plot for 2nd difference")))

dif3<-diff(dif2,differences = 1)
plot(dif3,type='o',ylab='Yt',main="Plot of 3rd difference")

acf_3rd<-acf(dif3, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf_3rd,main= expression(paste("ACF plot for 3rd difference")))

#Since negative we choose d=2
library(forecast)

ARMA_fit<-arima(dif2,order=c(2,0,0))
forecasted<-forecast(ARMA_fit,h=5)
plot(forecasted)

ARMA_original_fit<-auto.arima(time_series,d=2)
forecast_original<-forecast(ARMA_original_fit,h=5)
plot(forecast_original)
#Q4
cars<- read_csv("C:/4th_sem/CFM/For MSc, MTech/For MSc, MTech/Passenger cars clean.csv")
time_series<-ts(cars$Passengers)
plot(time_series,type='o',ylab="Average age", main='Average age of  the population by sex')


dif1<-diff(time_series,differences = 1)
plot(dif1,ylab='Yt',main="Plot of 1st difference")

dif2<-diff(dif1,differences = 1)
plot(dif2,ylab='Yt',main="Plot of 2nd difference")

dif3<-diff(dif2,differences = 1)
plot(dif3,ylab='Yt',main="Plot of 3rd difference")

acf_1st<-acf(time_series, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf_1st,main= expression(paste("ACF plot for time series")))


acf_1st<-acf(dif1, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf_1st,main= expression(paste("ACF plot for 1st difference")))

acf_2nd<-acf(dif2, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf_2nd,main= expression(paste("ACF plot for 2nd difference")))

acf_3rd<-acf(dif3, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf_3rd,main= expression(paste("ACF plot for 3rd difference")))

#Q5

electricity <- read_csv("C:/4th_sem/CFM/For MSc, MTech/For MSc, MTech/electricity.csv")

values<-ts(electricity$Value,frequency = 12)
#a
plot(values,xlab="Time",ylab="Values", main="Plot of Electricity over a time period")
#b
dif1<-diff(values,differences = 1)
plot(dif1,ylab='Yt',main="Plot of 1st difference")

#c
Seasonality<-diff(values,lag = 12)
plot(Seasonality,ylab='Yt',main="Plot of Differenced Seasonality")
#d
plot(dif1,type="l",col="red",ylab='Value',main="Time Series plot of Seaonal and 1st difference")
lines(Seasonality,col="green")
legend(x = "topleft", box.lwd = 1,box.lty = 1 , title="EQUATIONS",  cex = 0.5,
       legend=c("1st difference", "Seasonal Differenced"), 
       fill = c("Red","Green"))
acf.results<-acf(dif1, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("ACF Plot of 1st difference")))

#the value of 1st lag is small
acf.results<-acf(dif1, lag.max = 20, type = "partial",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("Partial ACF Plot of 1st difference")))

acf.results<-acf(Seasonality, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("ACF Plot of Seasonality")))

acf.results<-acf(Seasonality, lag.max = 20, type = "partial",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("Partial ACF Plot of Seasonality")))


#1 ARMA models
ARMA_seasonal<-arima(Seasonality,order = c(2,0,1))
ARMA_seasonal
forcast_seasonal<- forecast(ARMA_seasonal,h=12)
plot(forcast_seasonal,ylab="Electricity Consumption", xlab='Time')

ARMA_seasonal_1st<-arima(Seasonality, order=c(2,1,1))
forecast_seasonal_1st<-forecast(ARMA_seasonal_1st,h=12)
plot(forecast_seasonal_1st,ylab="Electricity Consumption", xlab='Time')

ARMA_seasonal
forcast_seasonal<- forecast(ARMA_seasonal,h=12)
plot(forcast_seasonal,ylab="Electricity Consumption", xlab='Time')

ARMA_seasonal_1st<-arima(Seasonality, order=c(2,1,1))
forecast_seasonal_1st<-forecast(ARMA_seasonal_1st,h=12)
plot(forecast_seasonal_1st,ylab="Electricity Consumption", xlab='Time')

SARMA<-Arima(Seasonality,order=c(0,1,1), seasonal=c(0,1,1))
SARMA_forcast<-forecast(SARMA,h=12)
plot(SARMA_forcast)