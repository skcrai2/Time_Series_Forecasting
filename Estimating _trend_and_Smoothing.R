library("TSA")
library("forecast")


### Non Stationary process types ####

#### Simulated data with linear trend

### Process with linear trend ####
x1=c()
a=0.32
for(t in 1:100){
  x1[t]=a*t+rnorm(1)
}
x.ts=ts(data=x1,frequency = 1, start = 1959) # change the data in time series object
ts.plot(x.ts)
fit.sim<-tslm(x.ts~ trend)
ts.plot(x.ts)
lines(fitted(fit.sim),lwd=2,col=2)
plot(fit.sim$residuals)
acf(fit.sim$residuals) # all acf's at non zero lags are zero.

### Air passenger data ######

data(AirPassengers) 
plot(AirPassengers)
structure(AirPassengers) # To see the structure of the data set. As you can see in the output it is a monthly data from 1994-2004.

# Fitting a linear trend a+bt
fit1<-tslm(AirPassengers~ trend)
attributes(fit1)
#Estimated regression coefficients
fit1$coefficients

plot.ts(AirPassengers)
lines(fitted(fit1),lwd=2,col=2)
#to plot residuals
plot(fit1$residuals) # Although we have removed the trend but the residuals are not stationary. WHY ???

#Fit a quadratic trend a+bt+bt^2
fit2<-tslm(AirPassengers~ trend+I(trend^2));fit2
plot.ts(AirPassengers)
lines(fitted(fit2),lwd=2,col=2)
### #to plot residuals##
plot(fit2$residuals)


###   Nonparametric filters::
wgt<-c(0.5,rep(1,11),0.5)/12  # How to choose that ???
fit3<-filter(AirPassengers,sides=2,filter=wgt)
plot.ts(AirPassengers)
lines(fit3,lwd=2,col=2)
### plot residuals ###
plot.ts(AirPassengers-fit3)


#Filtering Symetric weights
q=3 #Order of the filter.
wgts<-c(rep(1,(2*q+1)))/(2*q+1)  # How to choose that ???
fit4<-filter(AirPassengers,sides=2,filter=wgts)
plot.ts(AirPassengers)
lines(fit4,lwd=2,col=2)
### plot residuals ###
plot.ts(AirPassengers-fit4)

## Kernel Smoothing
fit5<-ksmooth(time(AirPassengers),AirPassengers,"normal",bandwidth = .25) # Using Gaussian kernel
plot.ts(AirPassengers)
lines(fit5,lwd=2,col=4)
## Residuals ###
plot(AirPassengers-fit5$y)

#### Exponential Smoothing ###
fit6<-HoltWinters(AirPassengers, alpha=0.62,beta=FALSE, gamma=FALSE)
plot(fit6)

