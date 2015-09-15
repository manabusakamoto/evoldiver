rhoBranch <- function(tree,rho){
	pat.dist <- dist.nodes(tree)[,length(tree$tip.label)+1]
	node.height <- data.frame(pat.dist)
	node.height <- max(pat.dist)-node.height
	node.height <- (node.height-min(node.height))/(max(node.height)-min(node.height))
	branch.height <- matrix(nrow=length(tree$edge[,1]),ncol=2)
	for(i in 1:length(tree$edge[,1])){
		node1 <- node.height[rownames(node.height)==tree$edge[i,1],]
		node2 <- node.height[rownames(node.height)==tree$edge[i,2],]
		branch.height[i,1] <- node1
		branch.height[i,2] <- node2
		}
	rho.node <- branch.height^rho
	rho.branch <- rho.node[,1]-rho.node[,2]
	tree$edge.length <- rho.branch
	return(tree)
}