# function to collapse sequences of ordered characters into dashed range, e.g., 1-35
ordChar <- function(x){
  n1 <- x[-1] - x[-length(x)]
  n1 <- c(0, n1)
  n2 <- x[-length(x)] - x[-1]
  n2 <- c(n2,0)
  
  x[n2==-1&n1==1] <- "-"
  
  oc <- NULL
  for(i in 1:length(x)){
    if(length(grep("-", x[i]))==0) oc <- c(oc, x[i])
    if(length(grep("-", x[i]))>0 & length(grep("-", x[i-1]))==0) oc <- c(oc, x[i])
  }
  oc <- paste(oc, collapse=" ")
  return(oc)
}