########################################################################
# function to calculate pairwise p-values between groups
pair.pMCMC <- function(x){
  pMCMC <- matrix(ncol=ncol(x), nrow=ncol(x))
  for(j in 1:ncol(x)){
    x1 <- x[,j]
    x2 <- x[,-j, drop=F]
    p <- numeric(ncol(x2))
    for(k in 1:ncol(x2)){
      d <- x1 - x2[,k]
      cond1 <- d > 0
      cond2 <- d < 0
      p[k] <- min(c({sum(cond1)/length(d)},{sum(cond2)/length(d)}))*2
    }
    if(j==1){
      p <- c(NA, p)
    }else{
      if(j==ncol(x)){
        p <- c(p, NA)
      }else{
        p <- c(p[1:{j-1}], NA, p[j:length(p)])
      }
    }
    pMCMC[,j] <- p
  }
  return(pMCMC)
}
########################################################################