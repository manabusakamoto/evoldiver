#####	PTS3D	#####

PTS3D <- function(x, y, z, tree, radius=NULL, xlab=NULL, ylab=NULL, zlab=NULL, col=NULL, clade=NULL, A=NULL, method=c("REML","ML","PIC"), node.label=T, node.adj=NULL, node.cex=NULL, tip.label=F, tip.adj=NULL, tip.cex=NULL, box=T){
	require(ape)
	require(rgl)
	if(is.null(col)){col=rep("black",length(tree$tip))}
	if(is.null(radius)){radius=0.2}
	if(is.null(xlab)){xlab="X"}
	if(is.null(xlab)){xlab="X"}
	if(is.null(ylab)){ylab="Y"}
	if(is.null(zlab)){zlab="Z"}
	if(is.null(A)){
	  if(method=="REML"){
	    reml.x <- ace(x,tree,method="REML")
	    reml.y <- ace(y,tree,method="REML")
	    reml.z <- ace(z,tree,method="REML")
	    ace.x <- reml.x$ace
	    ace.y <- reml.y$ace
	    ace.z <- reml.z$ace
	  }
		if(method=="ML"){
			ml.x <- ace(x,tree,method="ML")
			ml.y <- ace(y,tree,method="ML")
			ml.z <- ace(z,tree,method="ML")
			ace.x <- ml.x$ace
			ace.y <- ml.y$ace
			ace.z <- ml.z$ace
			}
		if(method=="PIC"){
			pic.x <- ace(x,tree,method="pic")
			pic.y <- ace(y,tree,method="pic")
			pic.z <- ace(z,tree,method="pic")
			ace.x <- pic.x$ace
			ace.y <- pic.y$ace
			ace.z <- pic.z$ace
			}
		}else{ace.x<-A[,1];ace.y<-A[,2];ace.z<-A[,3]}
	if(length(col)==1){col=rep(col,length(tree$tip))}else{col=col}
	if(is.null(clade)){nodeCol <- rep("gray", tree$Nnode)
		node.col <- cbind(seq(1:tree$Nnode),nodeCol)}else{
		node.col <- cladeCol(tree,clade,col)}
	allnodecol <- c(col,as.character(node.col[,2]))
	X <- c(x,ace.x)
	names(X)[1:length(x)] <- c(1:length(x))
	Y <- c(y,ace.y)
	names(Y)[1:length(y)] <- c(1:length(y))
	Z <- c(z,ace.z)
	names(Z)[1:length(z)] <- c(1:length(z))
	col3d <- c(col,node.col[,2])
	names(col3d)[1:length(col)]<-c(1:length(col))
	open3d()
	plot3d(x,y,z,col=col, type="s",radius=radius, xlab="", ylab="", zlab="", xlim=c(min(X), max(X)), ylim=c(min(Y), max(Y)), zlim=c(min(Z), max(Z)), box=F)
	spheres3d(ace.x, ace.y, ace.z, radius=radius/2, col=node.col[,2])
	decorate3d(xlab=xlab, ylab=ylab, zlab=zlab, box=box)
	nodes.x <-(X)
	nodes.y <-(Y)
	nodes.z <-(Z)
	connectBranch3D(tree, nodes.x, nodes.y, nodes.z, allnodecol,alpha=NULL)
	if(node.label==T){
		if(is.null(node.adj)){node.adj=1.5}
		if(is.null(node.cex)){node.cex=0.5}
			labels <- names(ace.x)
			text3d(ace.x, ace.y, ace.z, col=node.col[,2], text=labels, adj=node.adj, cex=node.cex)
		}
	if(tip.label=="num"){
		if(is.null(tip.adj)){tip.adj=1}
		if(is.null(tip.cex)){tip.cex=0.6}
			text3d(x, y, z, col=col, text=seq(1:length(x)), adj=tip.adj, cex=tip.cex)
		}
	if(tip.label=="tip"){
		if(is.null(tip.adj)){tip.adj=1}
		if(is.null(tip.cex)){tip.cex=0.6}
			text3d(x, y, z, col=col, text=tree$tip, adj=tip.adj, cex=tip.cex)
		}
}


#####	Functions by Manabu Sakamoto (m.sakamoto@bristol.ac.uk)