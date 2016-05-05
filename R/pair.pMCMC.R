########################################################################
# function to calculate pairwise p-values between groups
pair.pMCMC <- function(x){
  x <- as.data.frame(x)
  pMCMC <- matrix(ncol=ncol(x), nrow=ncol(x))
  for(j in 1:ncol(x)){
    x1 <- x[,j]
    x2 <- x[,-j, drop=F]
    p <- numeric(ncol(x2))
    for(k in 1:ncol(x2)){
      d <- x1 - x2[,k]
      if(mean(d)>0){.cond <- d<=0}
      if(mean(d)<0){.cond <- d>=0}
      p[k] <- sum(.cond)/length(d)
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
  colnames(pMCMC) <- names(x)
  rownames(pMCMC) <- names(x)
  return(pMCMC)
}
########################################################################
