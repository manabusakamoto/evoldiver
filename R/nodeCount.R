nodeCount <- function(phy){
  phy$edge.length <- rep(1, length(phy$edge.length))
  nodes <- dist.nodes(phy)[Ntip(phy)+1,1:Ntip(phy)]
  names(nodes) <- phy$tip.label
  return(nodes)
}