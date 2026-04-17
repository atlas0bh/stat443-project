library(forecast)
library(IRdisplay)
library(smooth)
library(forecast)
library(languageserver)
source("./df.R")

source("transformations.R")

out_of_sample_rmse <- function(data,orders,sse_to_beat = Inf,inverse_transformation=expm1) {
    h <- 104
    n <- length(data)
    test_idx <- (n - h + 1):n
    sse <- 0
    previous_fit <- NULL
    fc <- numeric(length=h)
    for (i in seq_along(test_idx)) {
        display_text(paste("Complete: ",round(i/length(test_idx)*100) ,"%", "\n"))
        t_idx <- test_idx[i]
        train_i <- data[1:(t_idx - 1)]
        fit_i <- msarima(
            train_i,
            model = previous_fit,
            orders = orders,
            lags = c(1, 52, 19),
        )
        previous_fit <- fit_i
        fc_i <- forecast(fit_i, h = 1)
        fc[i] <- fc_i$mean[1]
        sse <- sse + (inverse_transformation(data[t_idx])-inverse_transformation(fc_i$mean[1]))^2
        if (sse > sse_to_beat) {
            return (list(rmse = Inf, forecasts = numeric(length=h),fit = fit_i))
        }
        clear_output()
    }
    return (list(rmse = sqrt(sse/h), forecasts = inverse_transformation(fc), close_fit = previous_fit))
}
# Now we want a function to vary the orders to see if we can do any better!!

perturb_orders <- function(orders, perturbation) {
    perturbed_orders <- lapply(orders, function(order) {
        order <- as.integer(order)
        order + sample(seq(-perturbation, perturbation), length(order), replace = TRUE)
    })
    perturbed_orders <- lapply(perturbed_orders, function(order) pmax(order, 0))
    names(perturbed_orders) <- names(orders)
    return(perturbed_orders)
}

order_string <- function(orders) {
    paste0(
        "ar=c(", paste(orders$ar, collapse = ", "), ")",
        ", i=c(", paste(orders$i, collapse = ", "), ")",
        ", ma=c(", paste(orders$ma, collapse = ", "), ")"
    )
}
find_best_rmse_out_of_sample <- function(data, initial_orders, perturbation, iterations,inverse_transformation=expm1) {
    results <- out_of_sample_rmse(data, orders = initial_orders, inverse_transformation = inverse_transformation)
    best_rmse <- results$rmse
    best_fc <- results$forecasts
    current_orders <- initial_orders
    fit <- results$close_fit
    best_sse <- best_rmse^2 * 104  # Convert RMSE back to SSE for comparison
    for (i in 1:iterations) {
        # for each itteration we want to perturb if the to try and improve the rmse on the out of sample data.
        # we optimise our search such that when we are calculating the sse along the one step rolling forecast, 
        # if we exceed the best sse so far then we must have a worse rmse and we can stop! This speeds up our search as
        # refitting the model is expensive part of the search process.
        clear_output(wait = TRUE)
        new_orders <- perturb_orders(current_orders, perturbation)
        results <- out_of_sample_rmse(data, orders = new_orders,sse_to_beat = best_sse,inverse_transformation = inverse_transformation)
        # display_text(paste("Processing iteration:", i))
        
        if (results$rmse < best_rmse) {
            best_fc <-results$forecasts
            best_rmse <- results$rmse
            fit <- results$close_fit
            best_sse <- best_rmse^2 * 104  # Update the best SSE for the next comparison
            current_orders <- new_orders
        }
        
        display_text(paste("Complete: ",round(i/iterations*100) ,"%", "Best Orders:", order_string(current_orders), "Best RMSE so far:", best_rmse, "\n"))
    }
    return(list(best_orders = current_orders, best_rmse = best_rmse,best_fc=best_fc, fit=fit))
}
