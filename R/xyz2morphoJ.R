#####	Function xyz2morphoJ
#####	Assemble XYZ coordinates into morphoJ file format

xyz2morphoJ <- function(x){
	M <- matrix(nrow=length(unique(x[,1])),ncol=3*length(x[,1])/length(unique(x[,1])))
	for(i in 1:length(unique(x[,1]))){
		sp <- as.matrix(x[x[,1]==unique(x[,1])[i],-1])
		sp.v <- c(sp[1,])
		while(length(sp.v) < 3*length(sp[,1])){
		new <- (length(sp.v)/3)+1
		sp.v <- c(sp.v,sp[new,])
		}
	M[i,] <- sp.v 
	}
	rownames(M) <- unique(x[,1])
	return(M)
}