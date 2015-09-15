#####  Size adjustment  #####
sizeAdj <- function(x,method="GM"){
	X <- x
	G <- exp(rowMeans(log(X),na.rm=T))
	if(method=="GM"){
		R <- X/G
		return(R)
		}
	if(method=="logGM"){
		R <- log(X,10) - log(G,10)
		return(R)
		}
	if(method=="res"){
		R <- matrix(nrow=length(X[,1]),ncol=length(X[1,]))
		for(i in 1:length(X[1,])){
			LM <- lm(X[,i]~G)
			R[,i] <- LM$residuals
			}
		R <- data.frame(R,row.names=row.names(X))
		colnames(R)<-colnames(X)
		return(R)
		}
	if(method=="log_res"){
		R <- matrix(nrow=length(X[,1]),ncol=length(X[1,]))
		for(i in 1:length(X[1,])){
			LM <- lm(log(X[,i],10)~log(G,10))
			R[,i] <- LM$residuals
			}
		R <- data.frame(R,row.names=row.names(X))
		colnames(R)<-colnames(X)
		return(R)
		}
	if(method=="MQ"){
		R <- matrix(nrow=length(X[,1]),ncol=length(X[1,]))
		for(i in 1:length(X[1,])){
			LM <- lm(log(X[,i],10)~log(G,10))
			Q <- (X[,i]/(10^LM$fitted))*100
			R[,i] <- round(Q,0)
			}
		R <- data.frame(R,row.names=row.names(X))
		colnames(R)<-colnames(X)
		return(R)
		}
}