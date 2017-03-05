# function to calculate heritability/lambda
h2.MCMCglmm <- function(tree, model, tip.label, ...){
  time.mean <- mean(dist.nodes(tree)[,length(tree$tip)+1])
  vcvnam <- tip.label
  pvcv <- model$VCV
  h2 <- {time.mean * pvcv[,vcvnam]} / {{time.mean * pvcv[,vcvnam]} + rowSums(pvcv[,which(!grepl(vcvnam, colnames(pvcv))), drop=FALSE])}
  hist(h2, xlim=c(0,1), xaxs="i", yaxs="i", ...)
}