#####	Function geoMean	#####
#####	compute geometric mean from matrix of variables	#####

geoMean <- function(x){
	X <- x
	G <- exp(rowMeans(log(X),na.rm=T))
	names(G) <- row.names(X)
	return(G)
}

