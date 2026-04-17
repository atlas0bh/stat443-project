library(forecast)
library(IRdisplay)
library(smooth)

source("df.R")
source("find_best_rmse.R")
extract_stl_components <- function(stl_object) {
  components <- list(
    trend = as.ts(stl_object$time.series[, "trend"]),
    seasonal = as.ts(stl_object$time.series[, "seasonal"]),
    remainder = as.ts(stl_object$time.series[, "remainder"]),
    original = stl_object$time.series[, "seasonal"] + stl_object$time.series[, "trend"] + stl_object$time.series[, "remainder"]
  )
  return(components)
}


stl_rolling_forecast <- function(train_data, holdout_data, s.window = "periodic", t.window = NULL,transformation=function(x)x) {
  forecasts <- numeric(length(holdout_data))
  residuals <- numeric(length(holdout_data))
  current_train <- train_data
  freq <- frequency(train_data)
  first_fit_resid<-NULL
  for (i in seq_along(holdout_data)) {
    display_text(paste("STL is rolling: ",round(i/length(holdout_data)*100) ,"%"))
    if (s.window == "periodic") {
      current_train_ts <- ts(current_train, frequency = freq)
    } else {
      current_train_ts <- current_train
    }
    stl_fit <- stl(current_train_ts, s.window = s.window, t.window = t.window)
    if(is.null(first_fit_resid)){
      first_fit_resid <- extract_stl_components(stl_fit)$remainder
    }
    forecasts[i] <- forecast(stl_fit, h = 1)$mean[1]
    residuals[i] <- transformation(holdout_data[i]) - transformation(forecasts[i])
    current_train <- c(current_train, holdout_data[i])
    clear_output(wait = TRUE)
  }
  #note we return the residuals of the first fit as well as the holdout set so we can pretend as though we are fitting as we load the data!
  #ie as we roll the forecast (but still having a generalisable model)
  return(list(
    forecasts = transformation(forecasts),
    residuals_train = first_fit_resid,
    residuals_holdout = residuals,
    actual = holdout_data,
    rmse = sqrt(mean(residuals^2))
  ))
}


stl_arima <- function(colname, orders, s.window = "periodic", t.window = NULL) {
  
  data         <- split_data(df[[colname]], remove.missing = TRUE)
  holdout_data <- data$holdout
  train_data   <- data$train
  freq         <- frequency(train_data)
  
  forecasts_in_nonlogspace        <- numeric(length(holdout_data))
  logresiduals        <- numeric(length(holdout_data))
  current_train    <- train_data
  ms_model         <- NULL

  for (i in seq_along(holdout_data)) {
    display_text(paste("STL+MSARIMA rolling:", round(i / length(holdout_data) * 100), "%"))


    current_train_ts <- ts(current_train, frequency = freq)
    stl_fit          <- stl(current_train_ts, s.window = s.window, t.window = t.window)
    stl_remainder    <- extract_stl_components(stl_fit)$remainder


    if (is.null(ms_model)) {

      ms_model <- msarima(
        stl_remainder,
        orders = orders,
        lags   = c(1, 52, 19)
      )
    } else {
#refit for fast convergence
      ms_model <- msarima(
        stl_remainder,
        orders = orders,
        lags   = c(1, 52, 19),
        model  = ms_model
      )
    }
    stl_fc        <- forecast(stl_fit, h = 1)$mean[1]
    ms_fc         <- forecast(ms_model, h = 1)$mean[1]
    sigma2 <- var(residuals(ms_model)) + var(as.numeric(stl_remainder))
    

    stl_components <- extract_stl_components(stl_fit)
    trend_fc <- tail(stl_components$trend, 1)
    seasonal_fc <- stl_components$seasonal[length(stl_components$seasonal) - frequency(train_data) + 1]
    

    forecasts_in_nonlogspace[i]  <- expm1(trend_fc + seasonal_fc + ms_fc + 1/2* sigma2)
    logresiduals[i]  <- holdout_data[i] - (trend_fc + seasonal_fc + ms_fc)


    current_train <- c(current_train, holdout_data[i])
    clear_output(wait = TRUE)
  }

  return(list(
    forecasts         = forecasts_in_nonlogspace,
    residuals_holdout = logresiduals,
    actual            = expm1(holdout_data),
    rmse              = sqrt(mean((expm1(holdout_data) - forecasts_in_nonlogspace)^2))
  ))
}