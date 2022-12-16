library(TSA)   # Load tsa package package 
library(forecast)
data("co2",package = "datasets")
xt=co2
plot(xt)
tsdisplay(xt) # It will display ACF and PACF is a single window.
# Decompose the time series in to trend seasonal component using additive (classical) model:
# Additive model means Xt=trend+seasonal+random
decomp.x.ad=decompose(xt,type = "additive")
plot(decomp.x.ad)
periodogram(decomp.x.ad$seasonal) # Note there is a peak at frequency=1/12 which means period is 12.

# Decompose the time series in to trend seasonal component using multiplicative (BOX) model:
# Additive model means Xt=trend * seasonal * random.
decomp.x.mul=decompose(xt,type = "multiplicative")
plot(decomp.x.mul)
periodogram(decomp.x$seasonal) ## Note there is a peak at frequency=1/12 which means period is 12.

## Another function is available for decomposition

decomp.x=stl(xt,s.window ='per')
plot(decomp.x)

### Another example ###

plot(EuStockMarkets[,'SMI'])
dec=decompose(EuStockMarkets[,'SMI'])
acf(EuStockMarkets[,'SMI'])
plot(dec)

