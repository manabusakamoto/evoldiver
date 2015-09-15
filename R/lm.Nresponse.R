lm.Nresponse <- function(x, y=NULL){
	if(is.null(y)){X <- x[,1];Y<-x[,-1]}else{
	X<-as.numeric(x)
	Y<-as.matrix(y)}
	modstats <- matrix(ncol=2,nrow=ncol(Y))
	for(i in 1:ncol(Y)){
		mods <- summary(lm(Y[,i]~X))
		r2 <- mods$r.squared
		p <- pf(mods$fstatistic[1], mods$fstatistic[2], mods$fstatistic[3], lower.tail = FALSE)
		modstats[i,]<-c(r2,p)
	}
	colnames(modstats)<-c("r.squared","p.value")
	rownames(modstats)<-colnames(Y)
	return(modstats)
}


#####	Function by Manabu Sakamoto, 2012: manabu.sakamoto@gmail.com