#####  function nodeAges
nodeAges <- function(tree, tip.age=NULL, min.age=NULL){
	pairdist <- dist.nodes(tree)[,length(tree$tip.label)+1]
	nodedist <- pairdist[-c(1:length(tree$tip.label))]
	termdist <- pairdist[-c((length(tree$tip.label)+1):length(pairdist))]
	distMin <- min(termdist)
	if(is.null(tip.age)){ages <- max(pairdist) - pairdist
		if(is.null(min.age)){ages <- ages}else{ages <- ages + min.age}
		tip.age <- ages[1:length(tree$tip.label)]
		nodeage <- ages[-c(1:length(tree$tip.label))]
		}else{nodeage <- (max(tip.age) + distMin) - nodedist}
	allages <- list(tip.age=tip.age, nodeage=nodeage)
	return(allages)
}

#####	Functions by Manabu Sakamoto (m.sakamoto@bristol.ac.uk)