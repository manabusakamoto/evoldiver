#####	rarefy disparity metrics	#####
##	x is data, method is bootstrap or jack-knife, repet is 
##	number of repetition, e.g., 1000, but slows down 
##	calculation at large numbers, minN is minimum rarefied 
##	sample size (default is 1 but doesn't work for jack-knife),
##	and maxN is the maximum sample size (default is the maximum
##	number of rows for the dataset)
#####

rareDisp <- function(x, method="boot", repet, minN=NULL, maxN=NULL){
	X <- as.matrix(x)
	rareList <- list()
	if(is.null(minN)){minN <- 1}
	if(is.null(maxN)){maxN <- length(X[,1])}
	if(method=="jack"){
		for(i in minN:maxN){
			assign(paste("N=",i,sep=""),jackDisp(X,repet,n=i))
			rareList[[1+i-minN]] <- get(paste("N=",i,sep=""))
			names(rareList)[[1+i-minN]]<-paste("N=",i,sep="")
			cat(paste("N=",i,";",sep=""))
			}
		}else{
		for(i in minN:maxN){
			assign(paste("N=",i,sep=""),bootDisp(X,repet,n=i))
			rareList[[1+i-minN]] <- get(paste("N=",i,sep=""))
			names(rareList)[[1+i-minN]]<-paste("N=",i,sep="")
			cat(paste("N=",i,";",sep=""))
			}
		}
	return(rareList)
}

#####	function bootDisp
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


#####	function jackDisp
jackDisp <- function(x,repet,n=NULL){
	X <- data.frame(x)
	if(is.null(n)){n <- length(X[,1])}
	if(n > length(X[,1])){stop("Sample size n must not exceed number of rows in x")}else{
	rare <- matrix(nrow=repet,ncol=6)
	for(i in 1:repet){
		sample1 <- as.matrix(X[sample(1:nrow(X),n,replace=F),])
#		sample1 <- as.matrix(sample(X[,1],n,replace=F))
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