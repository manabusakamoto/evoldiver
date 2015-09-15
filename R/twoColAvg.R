	twoValAvg <- function(x, y){
		if(is.na(x)){
			if(is.na(y)){M <- NA}else{M <- y}
			}else{
				if(is.na(y)){M <- x}else{M <- (x + y)/2}
				}
		return(M)
	}
	
	twoColAvg <- function(x, y){
		V <- vector()
		for(i in 1:length(x)){
			V[length(V)+1] <- twoValAvg(x[i],y[i])
		}
		#V <- as.numeric(V)
		return(V)
	}
	
#####	using mean(c(x,y),na.rm=T) instead of twoValAvg	
#	twoColAvg <- function(x, y){
#		V <- vector()
#		for(i in 1:length(x)){
#			V[length(V)+1] <- mean(c(x[i],y[i]),na.rm=T)
#		}
#		#V <- as.numeric(V)
#		return(V)
#	}