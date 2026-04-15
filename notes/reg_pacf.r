
# sample data series for illustration
f = read.csv("furnace.csv",header=T)
y = f$furntemp
yts = ts(y,start=1,frequency=1)
pacfv = pacf(yts,plot=F,lag.max=5)$acf
print(c(pacfv))

h=2 # lag 2
ymat = ts.intersect(yts,lag(yts,1),lag(yts,2))
ydf = data.frame(ymat)
names(ydf)=paste("y",0:h,sep="")
reg1 = lm(y0~y1,data=ydf)
reg2 = lm(y2~y1,data=ydf)
alpha2 = cor(reg1$residuals,reg2$residuals)
print(alpha2)

h=3 # lag 3
ymat = ts.intersect(ymat,lag(yts,3))
ydf = data.frame(ymat)
names(ydf)=paste("y",0:h,sep="")
reg1 = lm(y0~y1+y2,data=ydf)
reg2 = lm(y3~y1+y2,data=ydf)
alpha3 = cor(reg1$residuals,reg2$residuals)
print(alpha3)

# function to get approximate pacf vector for time series vector y
reg_pacf = function(y, maxlag=3)
{ if(maxlag<2) return(0)
  n = length(y)
  pacfv = rep(0,maxlag)
  pacfv[1] = cor(y[-1],y[-n])
  yts = ts(y,start=1,frequency=1)
  ymat = ts.intersect(yts,lag(yts,1),lag(yts,2))
  reg1 = lsfit(ymat[,2],ymat[,1])
  reg2 = lsfit(ymat[,2],ymat[,3])
  alpha = cor(reg1$residuals,reg2$residuals)
  cat("alpha",2, alpha,"\n")
  pacfv[2] = alpha 
  for(h in 3:maxlag)
  { ymat = ts.intersect(ymat,lag(yts,h))
    reg1 = lsfit(ymat[,2:h],ymat[,1])
    reg2 = lsfit(ymat[,2:h],ymat[,h+1])
    alpha = cor(reg1$residuals,reg2$residuals)
    cat("alpha",h, alpha,"\n")
    pacfv[h] = alpha 
  }
  pacfv
}
  
pacf_approx = reg_pacf(y)

print(pacf_approx)
