# function to create colour gradients
colGradient <- function(x, n=NULL, col.range=NULL){
  if(is.null(col.range)) col.range <- c("hotpink", "dodgerblue")
  if(is.null(n)) n <- 10
  colPal <- colorRampPalette(col.range)
  col.grad <- colPal(n)[as.numeric(cut(x, breaks = n))]
  return(col.grad)
}