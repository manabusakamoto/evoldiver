##### Function intnodeage:
intnodeage <- function(tree,age){
	require(ape)
	pat.dist <- dist.nodes(tree)[,length(tree$tip.label)+1]
	pat.dist <- data.frame(pat.dist)
	age1 <- dist.nodes(tree)[1,length(tree$tip.label)+1]
	allnodeages <- date.nodes(tree,(max(age)+age1))
	allnodeages <- data.frame(allnodeages)
	int.nodeages <- allnodeages[-c(1:length(tree$tip.label)),]
	int.nodeages <- data.frame(int.nodeages)
	rownames(int.nodeages) <- c((length(tree$tip.label)+1):length(pat.dist[,1]))
	colnames(int.nodeages) <- "AGE"
	return(int.nodeages)
}