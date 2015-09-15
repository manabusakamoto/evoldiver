#####	plotRare
plotRare <- function(X, metric="mean", CI=TRUE, col=NULL, label=NULL){
	if(is.null(label)){label<-"Disparity"}
	if(is.null(col)){col=rep("black",length(X))}
		col <- col
	if(metric=="median"){M <- 2}else{
		M <- 1}
	YLimits <- matrix(nrow=length(X),ncol=2)
	for(i in 1:length(X)){
		minD <- min(X[[i]][,3])
		maxD <- max(X[[i]][,4])
		YLimits[i,] <- c(minD,maxD)
		Xmax <- length(X[[i]][,1])
		}
	Ymax <- max(YLimits); Ymin <- min(YLimits)
	if(CI == TRUE){
		plot(X[[1]][,M], xlim=c(0,Xmax),ylim=c(Ymin,Ymax), type="l", lwd=2, ylab=label,col=col[1])
		lines(X[[1]][,3],lty=2,col=col[1])
		lines(X[[1]][,4],lty=2,col=col[1])
		for(i in 2:length(X)){
			disp <- X[[i]][,M]
			CI.L <- X[[i]][,3]
			CI.U <- X[[i]][,4]
			lines(disp,lwd=2,col=col[i])
			lines(CI.L,lty=2,col=col[i])
			lines(CI.U,lty=2,col=col[i])
			}
		}else{
		plot(X[[1]][,M], xlim=c(0,Xmax),ylim=c(Ymin,Ymax), type="l", lwd=2, ylab=label)
		for(i in 2:length(X)){
			disp <- X[[i]][,M]
			CI.L <- X[[i]][,3]
			CI.U <- X[[i]][,4]
			lines(disp,lwd=2,col=col[i])
			}
		}
}
