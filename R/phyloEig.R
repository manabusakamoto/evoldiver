#####  Phylogenetic eigenvectors  #####  
phyloEig <- function(tree,proportion){
	require(ape)
	distphylo <- cophenetic.phylo(tree)
	distmat <- as.dist(distphylo,diag=T,upper=T)
	pco <- cmdscale(distmat, k=sqrt(length(as.matrix(distmat)))-1, add=T, eig=T)
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
	prop.var <- vector(mode="numeric")
	for(i in 1:length(cumvarpct)){
		prop.var[i]<-sum(cumvarpct[1:i])
		} 
	cutoff <- vector(mode="numeric")
	for(i in 1:length(prop.var)){
		if(prop.var[i]<=(proportion+1)){
			cutoff[i]<-prop.var[i]}
		}
	pco.scores2 <- pco.scores[,-c((length(cutoff)+1):length(pco.scores[,1]))]
	pco.scores2 <- data.frame(pco.scores2)
	for(i in 1:length(pco.scores2[1,])){
		colnames(pco.scores2)[i] <- paste("PCo",i,sep="")
		}
	return(pco.scores2)  	
}