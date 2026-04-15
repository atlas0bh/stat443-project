df <- read.csv("data/fluview_clean/ilinet_final.csv")

split_data <- function(df_column, frequency = 52, start = c(1997, 40), holdout_length = 104) {
  time_series_obj <- ts(df_column, frequency = frequency, start = start)
  n <- length(time_series_obj)
  return(list(
    train = window(time_series_obj, start = start, end = time(time_series_obj)[n - holdout_length]),
    holdout = window(time_series_obj, start = time(time_series_obj)[n - holdout_length + 1], end = time(time_series_obj)[n]),
    ts_obj = time_series_obj
  ))
}


values <- split_data(log(df$percent_weighted_ili + 1) - mean(log(df$percent_weighted_ili + 1)))


weeks <- split_data(df$week, frequency = 52, start = c(1997, 40), holdout_length = 104)

week_train <- rep(1:12, 18)
week_holdout <- rep(1:12, 5)
week_all <- rep(1:12, 23)
t_train <- 1:length(values$train)
t_holdout <- (length(values$train) + 1):(length(values$ts_obj))
t_all <- 1:length(values$ts_obj)

(percent_weighted_ili <- values$ts_obj)

train <- values$train
holdout <- values$holdout
