

print_rmse<- function(rmse,response_variable_name,explanatory_variable_names){
  print(paste("RMSE:", rmse, "for variable:",  response_variable_name, "with predictors:", paste(explanatory_variable_names, collapse = ", ") ))
}
