# function to identify invariant sites
invSite <- function(x){
  .x <- x[!grepl("\\?", x)]
  if(sum(grepl("[[:punct:]]", .x))>0){
    result <- FALSE
  }else{
    result <- length(unique(.x))==1
  }
  return(result)
}