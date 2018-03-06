axesBuffers <- function(x, y=NULL){
  if(is.null(y)){
    if(ncol(x)!=2) {
      stop("y is not specified ... OR, x must have two columns")
    }else{
      X <- as.matrix(x)
      x <- X[, 1]
      y <- X[, 2]
    }
  }
  .xlim <- range(x)
  .ylim <- range(y)
  buffer <- {.xlim[2] - .xlim[1]}*0.05
  .xlim[1] <- .xlim[1] - buffer
  .xlim[2] <- .xlim[2] + buffer
  buffer <- {.ylim[2] - .ylim[1]}*0.05
  .ylim[1] <- .ylim[1] - buffer
  .ylim[2] <- .ylim[2] + buffer
  return(list(xlim=.xlim, ylim=.ylim))
}

