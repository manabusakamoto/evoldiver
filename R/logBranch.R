logBranch <- function(tree,base){
	require(ape)
	tree$edge.length <- log(tree$edge.length,base)
	return(tree)
}