taxonChar <- function(x){
	M <- matrix(nrow=length(unique(x[,1])),ncol=length(x[1,-1]))
	for (i in 1:length(unique(x[,1]))){
		sp <- x[x[,1]==unique(x[,1])[i],-1]
		ch.sp <- matrix(nrow=1,ncol=length(x[1,-1]))
		for (j in 1:length(x[1,-1])){
			ch.sp[,j] <- as.character(sp[1,j])
			}
		M[i,] <- ch.sp
		}
	rownames(M) <- unique(x[,1])
	colnames(M) <- colnames(x[,-1])
	M <- data.frame(M)
	return(M)
}