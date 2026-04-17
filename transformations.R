

expm1 <- function(x) {
  exp(x) - 1
}
logp1 <- function(x) {
  log(1 + x)
}
percentage_transform <- function(x){
    return(log((x/100)/(1-x/100)))
}
inverse_percentage_transform <- function(x){
    return(100*exp(x)/(1+exp(x)))
}



# order cal look like this
# orders <- list(ar=c(1,52,19), i=c(0,0,0), ma=c(1,52,19))
# order<- list(arma=c(1,52,19),seasonal=c(1,52,19))
in_storage <- function(name){
  if (!file.exists("forecast_history.csv")) {
    return(NULL)
  }
  df <- read.csv("forecast_history.csv", stringsAsFactors = FALSE)
  if(nrow(df)>0){
    match <- df[df$name == name,]
    if(nrow(match)>0){
      forecasts_file <- paste0("forecasts_", name, ".rds")
      forecasts <- NULL
      if (file.exists(forecasts_file)) {
        forecasts <- readRDS(forecasts_file)
      }
      return (list(rmse = match$rmse[1], order = match$order[1], iterations = match$iterations[1], forecasts = forecasts))
    }
  }
  return(NULL)
}

set_storage <- function(name, order, rmse, iterations, forecasts = NULL){
  if (!file.exists("forecast_history.csv")) {
    df <- data.frame(name = character(), order = character(), iterations = numeric(), rmse = numeric(), stringsAsFactors = FALSE)
  } else {
    df <- read.csv("forecast_history.csv", stringsAsFactors = FALSE)
  }
  
  if(nrow(df)>0){
    match_idx <- which(df$name == name)
    if(length(match_idx) > 0){
      if(df$rmse[match_idx[1]] > rmse){
        # we update the rmse
        df$rmse[match_idx[1]] <- rmse
        df$order[match_idx[1]] <- order
        # Save forecasts if provided
        if (!is.null(forecasts)) {
          saveRDS(forecasts, paste0("forecasts_", name, ".rds"))
        }
      }
      write.csv(df, "forecast_history.csv", row.names = FALSE)
      return()
    }
  }
  new_row <- data.frame(name = name, order = order, iterations = iterations, rmse = rmse, stringsAsFactors = FALSE)
  df <- rbind(df, new_row)
  write.csv(df, "forecast_history.csv", row.names = FALSE)
  # Save forecasts if provided
  if (!is.null(forecasts)) {
    saveRDS(forecasts, paste0("forecasts_", name, ".rds"))
  }
}

save_img <- function(plotfcn,name,height,width){
  png(paste0(c(name,".png"),collapse=""), width = width, height = height)
  plotfcn()
  dev.off()
}