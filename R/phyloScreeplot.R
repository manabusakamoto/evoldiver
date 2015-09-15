#####  Plotting screeplot of phylogenetic eigenvectors  #####
phyloScreeplot <- function(tree,quartz=TRUE){
	require(ape)
	distphylo <- cophenetic.phylo(tree)
	distmat <- as.dist(distphylo,diag=T,upper=T)
	pco <- cmdscale(distmat, k=sqrt(length(as.matrix(distmat)))-1,add=T,eig=T)
	pco.scores <- pco$points
	for(i in 1:length(pco$points[1,])){
		if(pco$eig[i]<0){
			pco.scores <- pco.scores[,-i]
			}
		}
	rownames(pco.scores) <- tree$tip.label
	cumvar <- vector(mode="numeric")
	for (i in 1:length(pco.scores[1,])) cumvar[i]<-var(pco.scores[,i])
	cumvarpct<-(cumvar/sum(cumvar))*100
	barplot(cumvarpct, space=F, names.arg=c(1:length(pco.scores[1,])), cex.names=0.8)
}