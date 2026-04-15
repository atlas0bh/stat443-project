

expm1 <- function(x) {
  exp(x) - 1
}
logp1 <- function(x) {
  log(1 + x)
}
percentage_transform <- function(x){
    return(log((x/100)/(1-x/100)))
}
