set.seed(3)

x <- arima.sim(n=2000, model=list(ar=c(0.4,-0.2,0.6), ma=c(0.6,-0.4)))

plot(x)


# want model with lost AIC value. Find optimal Q and P values 
#for MA and AR

solution.aic <- Inf

solution.order <- c(0,0,0)

# Generate lots of ARMA models with different P and Q values
for (i in 1:4) for (j in 1:4) {
  
  actual.aic <- AIC(arima(x,order=c(i,0,j),optim.control = list(maxit=1000)))
  if (actual.aic < solution.aic){
    solution.aic <-actual.aic
    solution.order <- c(i,0,j)
    solution.arma <- arima(x,order=solution.order,optim.control = list(maxit=1000))
  }
  
}

solution.aic
solution.order
solution.arma
acf(resid(solution.arma))

Box.test(resid(solution.arma),lag=20,type='Ljung-Box')
