install.packages('quantmod')

require(quantmod)

getSymbols("^GSPC", src='yahoo')

returns <- diff(log(Ad(GSPC)))

solution.aic <- Inf
solution.order <- c(0,0,0)

for (i in 0:4) for (j in 0:4) {
  
  actual.aic <- AIC(arima(returns,order=c(i,0,j),optim.control = list(maxit=1000)))
  if (actual.aic < solution.aic){
    solution.aic <-actual.aic
    solution.order <- c(i,0,j)
    solution.arma <- arima(returns,order=solution.order,optim.control = list(maxit=1000))
  }
  
}
solution.order
#  found to be 3,2 (AR,MA)
acf(resid(solution.arma),na.action = na.omit)
#Found some considerable lag in the resid. Thus model not good.
#Lag (serial corrlation) in resid means model not explaining all the movements.
# This can be due to volatilty clustering.
Box.test(resid(solution.arma),lag=20,type='Ljung-Box')
# Tests autocorrelations betweens groups of lags.
# Null hypothesis is that time series at each lag has NO autocorrolation
# THUS if p < 0.05 REJECT nul and therefore IS serial corrlation.

#Since p was found to be 1.005e-05,must reject Null and accept model is bad.


