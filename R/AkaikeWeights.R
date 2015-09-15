### Function to compute Akaike weights:
AkaikeWeights <- function(x){
	wi <- signif(exp(-0.5*(x - min(x)))/sum(exp(-0.5*(x - min(x)))), digits=3)
	colnames(wi) <- colnames(x)
	return(wi)
}