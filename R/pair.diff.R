pair.diff <- function(x){
  df <- matrix(ncol = length(x), nrow = length(x))
  for(i in 1:length(x)) {
    # i <- 1
    .x <- c(x[[i]])
    df[, i] <- t(x - .x)
  }
  colnames(df) <- rownames(df) <- names(x)
  return(df)
}