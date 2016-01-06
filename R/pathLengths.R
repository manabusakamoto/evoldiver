pathLengths <- function(phy){
  nodes <- dist.nodes(phy)[Ntip(phy)+1,1:Ntip(phy)]
  names(nodes) <- phy$tip.label
  return(nodes)
}