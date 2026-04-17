

df <- read.csv("data/fluview_clean/ilinet_final.csv")
variogram = function(y, lagmax=10, iprint=F)
{ G = rep(1,lagmax)
  n = length(y)
  if(lagmax>n) { lagmax = n-2 }
  y1 = y[-1]; y2 = y[-n]
  d1 = y1-y2; denom = var(d1)
  for(k in 2:lagmax)
  { y1 = y[(k+1):n]; y2 = y[1:(n-k)]
    dk = y1-y2
    numer = var(dk)
    G[k] = numer/denom
  }
  # H is the variogram assuming a stationary time series
  H = rep(1,lagmax)
  ac = c(acf(y,plot=F,lag.max=lagmax)$acf)
  H = (1-ac[-1])/(1-ac[2])
  if(iprint) 
  { print(cbind(G,H)) 
    mx = max(cbind(G,H))
    matplot(1:lagmax,cbind(G,H),ylim=c(0,mx),ylab="variogram", xlab="lag")
    abline(h=1)
  }
  list(G=G, H=H)
}
# vgram_ar1neg = variogram(df$percent_weighted_ili,lagmax=52, iprint=T)


model_diagnostics <- function(model_fit,residuals,lag=52){
    tsdiag(model_fit, gof.lag=52)
    Box.test(residuals,lag, type="Ljung-Box")
}
aggregate_n_weeks <- function(ts_data, coarseness = 4){
    n <- length(ts_data)
    num_periods <- ceiling(n / coarseness)
    result <- numeric(num_periods)
    for (i in 1:num_periods) {
        start_idx <- (i - 1) * coarseness + 1
        end_idx <- min(i * coarseness, n)
        result[i] <- sum(ts_data[start_idx:end_idx])
    }
    return(ts(result, frequency = 13))  # 52 weeks / 4 = 13 periods per year
}

periodogram <- function(data_value,main=NULL,coarseness=4,span=c(5,7)){
    par(mfrow=c(2,1))
    spec.pgram(data_value,span=span,main=main)
    aggregate_n_weeks(data_value,coarseness) |> spec.pgram(span=span,main=paste0(c("Aggregate 4 weeks of",main),collapse=" "))
    par(mfrow=c(1,1))
}


residuals_diagnostics <- function(model_fit, residuals, lag=52){
    par(mfrow=c(2,1))
    variogram(residuals,lagmax=lag, iprint=T)
    qqnorm(residuals, main="QQ Plot of Residuals")
    qqline(residuals)
    par(mfrow=c(1,1))
    model_diagnostics(model_fit, residuals, lag)
}


plotresidues <- function(residuals, name) {
  par(mfrow=c(2,1))
  plot(residuals, type='l',main=paste("Residuals of", name), ylab="Residuals", xlab="Time")
  qqnorm(residuals, main=paste("QQ Plot of Residuals for", name))
  acf(residuals, main=paste("ACF of Residuals for", name),lag.max = 52)
  pacf(residuals, main=paste("PACF of Residuals for", name),lag.max = 52)
  par(mfrow=c(1,1))
}