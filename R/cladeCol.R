#####  function cladeCol
cladeCol <- function(tree,clade,col){
	clade.node <- matrix(nrow=tree$Nnode,ncol=1)
	rownames(clade.node) <- c((length(tree$tip.label)+1):((length(tree$tip.label))+tree$Nnode))
	for(i in 2:length(tree$tip.label)){
		if(clade[c(i-1)] == clade[i]){
			anc <- mrca(tree)[tree$tip.label[c(i-1)],tree$tip.label[i]]
			clade.node[rownames(clade.node)==anc,] <- as.character(clade[i-1])
			}else{
		anc <- mrca(tree)[tree$tip.label[c(i-1)],tree$tip.label[i]]
		clade.node[rownames(clade.node)==anc,]<-anc}
		}
	clade.col <- cbind(as.character(unique(clade)),unique(col))
	node.col <- matrix(nrow=tree$Nnode,ncol=2)
	rownames(node.col) <- c((length(tree$tip.label)+1):((length(tree$tip.label))+tree$Nnode))
	colnames(node.col) <- c("Clade","Col")
	for(i in 1:length(clade.node[,1])){
		if(clade.node[i,]%in%clade.col[,1]){
			node.col[i,] <- clade.col[clade.col[,1]%in%clade.node[i,],]
			}else{
		node.col[i,1]<-clade.node[i,]
		node.col[i,2]<-"gray"
		}
	}
	return(node.col)
}