
# Regression on the previous 2, 3, 4, ..., 20 observations
load("02kamloops.RData")
# ls()
#[1] "holdout"     "kamloops_ts" "train" 

# train is the training set for Kamloops daily meantemp

# Use ten years of daily data
yts = ts(train[1:3650],start=1,frequency=1)

maxlag = 20
ymat = ts.intersect(yts,lag(yts,1))
for(h in 2:maxlag)
{ ymat = ts.intersect(ymat,lag(yts,h))  # cols y_t, y_{t+1},...,y_{t+h}
  colnames(ymat) = paste("y",0:h+1,sep="")
  resp_var = paste("y",h+1,sep="")
  # regress y_{t+h} on y_{t},...,y_{t+h-1} 
  reg_lagh = lsfit(ymat[,1:h],ymat[,h+1])
  cat("\nlag", h, ": response variable is ", resp_var, "\n")
  ls.print(reg_lagh)
  # Check if  coefficient of coefficient of y_{t} is significant after
  #    including y_{t+1},...,y_{t+h}
  # If t-value for beta1 is smaller in absolute value, then
  # partial cor (y_t, y_{t+h} | y_{t+1},...,y_{t+h-1} ) is smaller in abs value.
  # Note that most significant beta is that for the previous observation y_{t+h-1}
}


# General result for multiple regression 
# y_i regress on x_{i1}, ..., x_{ij}, ..., x_{ip}

# If betahat_j is small in absolute value, then
# jth variable x_j (or x_{ij})  has little predictive value for y (or y_i)
# after x_1,...,x_{j-1},x_{j+1},...x_p
# or x_{i1},...,x_{i,j-1},x_{i,j+1},...x_{i,p} are already in model. 
# Then the partial correlation 
# rho(y,x_j;x_1,...,x_{j-1},x_{j+1},...x_p) is small in absolute value.

# Take j=1 below and regress on the past h values of the time series.

# The above regresses y_{i+h} on x_{i1}, ..., x_{ij}, ..., x_{ih}
# where x_{i1} = y_{i}, x_{i2}= y_{i+1}, ... x_{ih} = y_{i+h-1}
# If the first betahat_1 is small in absolute value, then
# y_{i} (at lag h) has little predictive value  for y_{i+h}
# after the intermediate values y_{i+1},...,y_{i+h-1} are already in the model.
# Then the partial autocorrelation 
# rho(y_{i+h},y_{i};y_{i+1},...,y_{i+h-1}) is small in absolute value.
