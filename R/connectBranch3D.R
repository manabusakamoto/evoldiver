#####	function to connect branches
connectBranch3D <- function(tree, nodes.x, nodes.y, nodes.z, col, alpha=NULL){
	if(is.null(alpha)){alpha=1}
	for(i in 1:length(tree$edge[,1])){
			node1 <- tree$edge[i,1]
			node2 <- tree$edge[i,2]
			node1.x <- nodes.x[names(nodes.x)==node1]
			node1.y <- nodes.y[names(nodes.y)==node1]
			node1.z <- nodes.z[names(nodes.z)==node1]
			node2.x <- nodes.x[names(nodes.x)==node2]
			node2.y <- nodes.y[names(nodes.y)==node2]
			node2.z <- nodes.z[names(nodes.z)==node2]
			segments3d(c(node1.x,node2.x), c(node1.y,node2.y), c(node1.z,node2.z), col=col[node1], alpha=alpha)}
}
#####	Functions by Manabu Sakamoto (m.sakamoto@bristol.ac.uk)