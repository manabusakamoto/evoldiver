
# calculate pMCMC of regression coefficients
pMCMC <- function(x, mu=NULL){
  if(is.null(mu)){mu <- 0}
  x <- data.table(x)
  p <- numeric(ncol(x))
  for(i in 1:ncol(x)){
    .b <- unlist(x[, i, with=F])
    if(mean(.b)>mu){.cond <- .b<=mu}
    if(mean(.b)<mu){.cond <- .b>=mu}
    if(mean(.b)==mu){.cond <- .b>=mu | .b<=mu}
    p[i] <- sum(.cond)/length(.b)
  }
  names(p) <- names(x)
  return(p)
}