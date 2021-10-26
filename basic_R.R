install.packages("quantmod")

require(quantmod)

getSymbols('AAPL',src='yahoo')

plot(Ad(AAPL))

aaplreturn = diff(log(Ad(AAPL)))

plot(aaplreturn)

acf(aaplreturn, na.action = na.omit)

aaplreturn.ma <- arima(aaplreturn,order=c(0,0,3))

aaplreturn.ma

acf(aaplreturn.ma$res[-1])
