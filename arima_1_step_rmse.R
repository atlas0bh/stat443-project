library(forecast)
source("transformations.r")
source("./df.R")
# install.packages("languageserver")
library(languageserver)
# we want to take an arima models


one_step_forecast_error <- function(training_data, holdout_data, training_model) {
  current_model <- training_model
  h <- length(holdout_data)
  sse <- 0
  order <- training_model$arma[c(1,6,2)]  
  fc_ <- numeric(length=h)
  seasonal <- list(order = training_model$arma[c(3,7,4)], period = training_model$arma[5])
  
  for (i in 1:h) {
    display_text(paste("ARIMA is rolling: ",round(i/h*100) ,"%"))

    fc <- forecast(current_model, h = 1)
    fc_error <- exp(as.numeric(holdout_data[i]))-1 - (exp(as.numeric(fc$mean[1]))-1)
    sse <- sse + as.numeric(fc_error)^2
    fc_[i] <- exp(as.numeric(fc$mean[1])) - 1
    
    # Refit with fast CSS method and limited optimization iterations
    updated_data <- ts(c(as.numeric(training_data), as.numeric(holdout_data)[1:i]), 
                       frequency = frequency(training_data))
    
    current_model <- tryCatch({
      Arima(updated_data, 
            order = order, 
            seasonal = seasonal,
            method = "CSS",
            optim.control = list(maxit = 50))
    }, error = function(e) {
      # Fallback to ML if CSS fails
      Arima(updated_data, 
            order = order, 
            seasonal = seasonal,
            method = "ML",
            optim.control = list(maxit = 50))
    })
  clear_output()
  }
  return(list(
    rmse = sqrt(sse / h),
    forecasts = fc_
  ))
}


# train <- data$train
# holdout <- data$holdout
# Arima(train,order = c(1,1,2),seasonal = list(order = c(2, 0, 0), period = 52))

# one_step_forecast_error(train, holdout, model)
# source("find_best_rmse.r")
out_of_sample_rmse <- function(data,orders,sse_to_beat = Inf,inverse_transformation=function(x){return(x)}) {
    h <- 104
    n <- length(data)
    test_idx <- (n - h + 1):n
    sse <- 0
    previous_fit <- NULL
    fc <- numeric(length=h)
    for (i in seq_along(test_idx)) {
        t_idx <- test_idx[i]
        train_i <- data[1:(t_idx - 1)]
        fit_i <- msarima(
            train_i,
            model = previous_fit,
            orders = orders,
            lags = c(1, 52,19),
        )
        previous_fit <- fit_i
        fc_i <- forecast(fit_i, h = 1)
        fc[i] <- fc_i$mean[1]
        sse <- sse + (inverse_transformation(data[t_idx])-inverse_transformation(fc_i$mean[1]))^2
        if (sse > sse_to_beat) {
            return (list(rmse = Inf, forecasts = numeric(length=h),fit = fit_i))
        }
    }
    return (list(rmse = sqrt(sse/h), forecasts = inverse_transformation(fc), close_fit = previous_fit))
}
# fc <- out_of_sample_rmse(data$ts_obj,orders=list(ar=c(2,2,2),i=c(1,0,1),ma=c(2,0,3)),inverse_transformation = expm1)
# fc$rmse
# ts.plot(holdout,fc$forecasts)





# library(forecast)

# k19 <- fourier(train, K = 2, period = 19) # 2 sin/cos pairs for period 19

# mod <- auto.arima(
#   train,
#   xreg = k19,
#   D = 1,
#   max.p = 1, max.q = 1,
#   max.P = 1, max.Q = 1,
#   stepwise = TRUE,
#   approximation = TRUE
# )


# n <- length(train)
# t <- 1:n
# period <- 19

# # Create sin/cos pairs manually (K=2 means 2 pairs)
# fourier_19 <- matrix(c(
#   sin(2 * pi * 1 * t / period),
#   cos(2 * pi * 1 * t / period)
# ), ncol = 2)

# colnames(fourier_19) <- c("s1_19", "c1_19", "s2_19", "c2_19", "s1_52", "c1_52", "s2_52", "c2_52")
# mod <- auto.arima(
#   train,
#   xreg = fourier_19,
#   max.p = 4, max.q = 4,
#   max.P = 1, max.Q = 1,
#   stepwise = TRUE,
#   approximation = TRUE,
#   trace = TRUE
# )
# t_holdout <- (n + 1):(n + length(holdout))

# fourier_19_holdout <- matrix(c(
#   sin(2 * pi * 1 * t_holdout / period),
#   cos(2 * pi * 1 * t_holdout / period),
# ), ncol = 2)

# colnames(fourier_19_holdout) <- c("s1_19", "c1_19", "s2_19", "c2_19", "s1_52", "c1_52", "s2_52", "c2_52")

# fc <- forecast(mod, xreg = fourier_19_holdout, h = length(holdout))
# plot(fc)
# sqrt(mean((holdout - fc$mean)^2))


# # TODO
# # ARIMA on single series

# # ARIMA on each series separately

# # Prediction of single series from the joint predictions.
# # How this will work is that since we have all the series and the forecasted values 
# # we will simply sum them to get back to the original series.
# # note since we actually have log(x+1) we must exponentiate each series and minus 1 from each and then add.
# # and then compare that to the holdout set of single series exponentiated and minus 1.
