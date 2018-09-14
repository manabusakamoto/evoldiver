pair.BayesFactor <- function(x){
  BF <- matrix(ncol = length(x), nrow = length(x))
  for(i in 1:length(x)) {
    # i <- 1
    .x <- x[i]
    BF[, i] <- BayesFactor(x, .x)
  }
  colnames(BF) <- rownames(BF) <- names(x)
  return(BF)
}
