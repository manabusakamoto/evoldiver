# reset taxa numbers
resetTipNum <- function(phy){
  cond <- is.element(phy$edge[,2], 0:Ntip(phy))
  phy$edge[cond, 2] <- phy$edge[cond, 2] + 1
  return(phy)
}