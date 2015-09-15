taxonMedian <- function(x){
	M <- matrix(nrow=length(unique(x[,1])),ncol=length(x[1,-1]))
	for (i in 1:length(unique(x[,1]))){
		sp <- x[x[,1]==unique(x[,1])[i],-1]
		med.sp <- matrix(nrow=1,ncol=length(x[1,-1]))
		for (j in 1:length(x[1,-1])){
			med.sp[,j] <- median(sp[,j],na.rm=T)
			}
		M[i,] <- med.sp
		}
	rownames(M) <- unique(x[,1])
	colnames(M) <- colnames(x[,-1])
	M <- data.frame(M)
	return(M)
}