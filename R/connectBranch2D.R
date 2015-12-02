#####	function to connect branches in 2D
connectBranch2D <- function(tree, nodes.x, nodes.y, col){
	for(i in 1:nrow(tree$edge)){
		node1 <- tree$edge[i,1]
		node2 <- tree$edge[i,2]
		node1.x <- nodes.x[rownames(nodes.x)==node1,]
		node1.y <- nodes.y[rownames(nodes.y)==node1,]
		node2.x <- nodes.x[rownames(nodes.x)==node2,]
		node2.y <- nodes.y[rownames(nodes.y)==node2,]
		segments(node1.x,node1.y,node2.x,node2.y, col=col[node1])
	}
}

#####	Functions by Manabu Sakamoto (m.sakamoto@bristol.ac.uk)