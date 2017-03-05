# function to create inverse A matrix
invA.MCMCglmm <- function(tree){
  return(inverseA(tree, scale=F)$Ainv)
}
