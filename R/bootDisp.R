bootDisp <- function(x,repet,n=NULL){
	X <- data.frame(x)
	if(is.null(n)){n <- length(X[,1])}
	if(n > length(X[,1])){warning("Sample size n should not exceed number of rows in x")}else{
	rare <- matrix(nrow=repet,ncol=6)
	for(i in 1:repet){
		sample1 <- as.matrix(X[sample(1:nrow(X),n,replace=T),])
#		sample1 <- as.matrix(sample(X[,1],n,replace=T))
		ranges <- vector(length=length(sample1[1,]))
		variances <- vector(length=length(sample1[1,]))
		for(j in 1:length(X[1,])){
			ranges[j] <- abs(max(sample1[,j])-min(sample1[,j]))
			}
		for(j in 1:length(X[1,])){
			if(length(sample1[,1])==1){variances[j]<-0}else{
			variances[j] <- sd(sample1[,j])^2
			}
		}
		sumranges <- sum(ranges)
		prodranges <- prod(ranges)
		kroot <- prodranges^(1/length(X[1,]))
		sumvar <- sum(variances)
		prodvar <- prod(variances)
		krootvar <- prodvar^(1/length(X[1,]))
		rare[i,] <- c(sumranges,prodranges,kroot,sumvar,prodvar,krootvar)
		}
	colnames(rare)<-c("SumRanges","ProdRanges","RootProdRanges","SumVar","ProdVar","RootProdVar")
	rare <- data.frame(rare)
	return(rare)
	}
}