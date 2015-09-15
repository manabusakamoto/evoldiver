#####	Lineage Density	#####
LineageDensity <- function(x,tree,method=c("D1","D2")){
	require(cluster)
	X <- as.matrix(x)
	EH <- ellipsoidhull(X)
	MBL <- morphBranchLength(X,tree)
	if(method=="D1"){
		L <- MBL$sum.morphometric.branch.length
		V <- volume(EH)
		D <- L/V
		}
	if(method=="D2"){
		L <- MBL$root.sum.morphometric.branch.length
		V <- sum(2*sqrt(diag(EH$cov)))*EH$d2
		D <- L/V
		}
	return(D)
}