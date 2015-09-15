#####	morphometric branch length	#####
morphBranchLength <- function(x,tree){
	X <- as.matrix(x)
	A <- matrix(nrow=tree$Nnode,ncol=length(X[1,]))
	for(i in 1:length(X[1,])){
		A[,i] <- ace(X[,i],tree)$ace
		}
	colnames(A)<-colnames(X)
	N <- rbind(X,A)
	n <- length(N[,1])
	rownames(N) <- seq(1:n)
	L.i <- vector(length=length(tree$edge[,1]))
	for(i in 1:length(tree$edge[,1])){
		node1 <- tree$edge[i,1]
		node2 <- tree$edge[i,2]
		node1.N <- N[rownames(N)==node1,]
		node2.N <- N[rownames(N)==node2,]
		L.j <- vector(length=length(N[1,]))
		for(j in 1:length(N[1,])){
			L.j[j] <- (node1.N[j]-node2.N[j])^2
			}
		L.i[i] <- sqrt(sum(L.j))
		}
	L <- sum(L.i)
	nL <- L^(1/length(X[1,]))
	results <- list(sum.morphometric.branch.length=L, root.sum.morphometric.branch.length=nL)
	return(results)
}