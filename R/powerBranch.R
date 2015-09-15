powerBranch <- function(tree,power){
	require(ape)
	tree$edge.length <- (tree$edge.length)^power
	return(tree)
}