threshold.pMCMC <- function(x){
  incr <- mean(pretty(abs(diff(range(x)))/100))
  sq <- seq(min(x), max(x), incr)
  
  p <- numeric(length(sq))
  for(i in 1:length(sq)){
    # i <- 1
    .sq <- sq[i]
    if(.sq < mean(x)){.cond <- x <= .sq}
    if(.sq > mean(x)){.cond <- x >= .sq}
    if(.sq == mean(x)){.cond <- x >= .sq | x <= .sq}
    p[i] <- sum(.cond)/length(x)
  }
  id <- which(p<0.05)
  id.s <- which(diff(id) > 1)
  id.l <- id.s+1
  p.s <- p[id[id.s]]
  p.l <- p[id[id.l]]
  thresholds <- c(Lower=sq[p==p.s], Upper=sq[p==p.l])
  return(thresholds)
}