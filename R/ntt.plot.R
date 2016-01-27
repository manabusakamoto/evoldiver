ntt.plot <- function(phy, log=T, ...){
  nodes <- nodeCount(phy)
  path.lengths <- pathLengths(phy)
  if(log){nodes <- log(nodes)}
  plot(nodes~path.lengths, ...)
}