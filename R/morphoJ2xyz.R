#####	Function morphoJ2xyz
#####	Convert morphoJ Procrustes coordinates to R readable XYZ coordinates

morphoJ2xyz <- function(x){
	x <- as.matrix(x)
	xyz.list <- list()
	for(i in 1:length(x[,1])){
		sp <- x[i,]
		sp <- unname(sp)
		xyz <- c(sp[1:3])
		sp <- sp[-c(1:3)]
			while(length(sp)>0){
			new <- sp[1:3]
			sp <- sp[-c(1:3)]
			xyz <- rbind(xyz,new)	
			}
		rownames(xyz) <- c(1:length(xyz[,1]))
		colnames(xyz) <- c("X","Y","Z")
		xyz.list[[i]] <- xyz
		}
	return(xyz.list)
}