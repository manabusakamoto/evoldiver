#####	Summarise rareDisp object	#####
##	x is rareDisp object of class list
##	CI.level is the percentile for confidence limits, e.g. 95
summaryDisp <- function(x,CI.level){
	X <- x
	low <- (100-CI.level)/200
	upp <- 1-low
	rareStats <- list()
	for(i in 1:length(X)){
		stats.i <- list()
		for(j in 1:length(X[[i]][1,])){
			meanDisp <- mean(X[[i]][,j])
			medianDisp <- median(X[[i]][,j])
			if(i == 1){CI <- c(0,0)}else{
				CI <- CI <- quantile(X[[i]][,j], c(low,upp))
				}
			stats.i[[j]] <- c(meanDisp,medianDisp,CI)
			}
		rareStats[[i]] <- stats.i
		}
	SR <- matrix(nrow=length(rareStats),ncol=length(rareStats[[1]][[1]]))
	PR <- matrix(nrow=length(rareStats),ncol=length(rareStats[[1]][[1]]))
	RPR <- matrix(nrow=length(rareStats),ncol=length(rareStats[[1]][[1]]))
	SV <- matrix(nrow=length(rareStats),ncol=length(rareStats[[1]][[1]]))
	PV <- matrix(nrow=length(rareStats),ncol=length(rareStats[[1]][[1]]))
	RPV <- matrix(nrow=length(rareStats),ncol=length(rareStats[[1]][[1]]))
	for(i in 1:length(rareStats)){
		SR[i,] <- rareStats[[i]][[1]]
		PR[i,] <- rareStats[[i]][[2]]
		RPR[i,] <- rareStats[[i]][[3]]
		SV[i,] <- rareStats[[i]][[4]]
		PV[i,] <- rareStats[[i]][[5]]
		RPV[i,] <- rareStats[[i]][[6]]
		}
	colnames(SR) <- c("Mean","Median","95%CI_Low","95%CI_Upp")
	colnames(PR) <- c("Mean","Median","95%CI_Low","95%CI_Upp")
	colnames(RPR) <- c("Mean","Median","95%CI_Low","95%CI_Upp")
	colnames(SV) <- c("Mean","Median","95%CI_Low","95%CI_Upp")
	colnames(PV) <- c("Mean","Median","95%CI_Low","95%CI_Upp")
	colnames(RPV) <- c("Mean","Median","95%CI_Low","95%CI_Upp")
	results <- list(SR,PR,RPR,SV,PV,RPV)
	names(results) <- c("Sum.of.Ranges", "Product.of.Ranges", "Root.Product.of.Ranges", "Sum.of.Variances", "Product.of.Variances", "Root.Product.of.Variances")
	return(results)
}
