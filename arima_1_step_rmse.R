library(forecast)
source("./df.R")
install.packages("languageserver")
# we want to take an arima models
plot(diff(train))
model <- auto.arima(diff(train))


one_step_forecast_error <- function(training_data, holdout_data, training_model) {
  current_model <- training_model
  h <- length(holdout_data)
  sse <- 0
  for (i in 1:length(holdout_data)) {
    fc <- forecast(current_model, h = 1)
    fc_error <- holdout_data[i] - fc$mean
    sse <- sse + fc_error^2
    current_model <- arima()
  }

  return(sqrt(mean(sse)))
}
mod <- auto.arima(train,
  stepwise = TRUE, # default, but make sure it's on
  approximation = TRUE, # uses faster approximation of likelihood
  max.P = 1, max.Q = 1, # limit seasonal AR/MA order
  max.p = 1, max.q = 1, # limit non-seasonal order
  D = 1, # fix seasonal differencing (you already diff manually)
  trace = TRUE
)

summary(mod)
as.numeric(residuals(mod))
residmod <- auto.arima(as.numeric(residuals(mod)),
  stepwise = TRUE, # default, but make sure it's on
  approximation = TRUE, # uses faster approximation of likelihood
  max.P = 1, max.Q = 1, # limit seasonal AR/MA order
  max.p = 1, max.q = 1, # limit non-seasonal order
  m = 19, # fix seasonal differencing (you already diff manually)
  trace = TRUE
)
summary(residmod)

library(forecast)

k19 <- fourier(train, K = 2, period = 19) # 2 sin/cos pairs for period 19

mod <- auto.arima(
  train,
  xreg = k19,
  D = 1,
  max.p = 1, max.q = 1,
  max.P = 1, max.Q = 1,
  stepwise = TRUE,
  approximation = TRUE
)


n <- length(train)
t <- 1:n
period <- 19

# Create sin/cos pairs manually (K=2 means 2 pairs)
fourier_19 <- matrix(c(
  sin(2 * pi * 1 * t / period),
  cos(2 * pi * 1 * t / period)
), ncol = 2)

colnames(fourier_19) <- c("s1_19", "c1_19", "s2_19", "c2_19", "s1_52", "c1_52", "s2_52", "c2_52")
mod <- auto.arima(
  train,
  xreg = fourier_19,
  max.p = 4, max.q = 4,
  max.P = 1, max.Q = 1,
  stepwise = TRUE,
  approximation = TRUE,
  trace = TRUE
)
t_holdout <- (n + 1):(n + length(holdout))

fourier_19_holdout <- matrix(c(
  sin(2 * pi * 1 * t_holdout / period),
  cos(2 * pi * 1 * t_holdout / period),
), ncol = 2)

colnames(fourier_19_holdout) <- c("s1_19", "c1_19", "s2_19", "c2_19", "s1_52", "c1_52", "s2_52", "c2_52")

fc <- forecast(mod, xreg = fourier_19_holdout, h = length(holdout))
plot(fc)
sqrt(mean((holdout - fc$mean)^2))
