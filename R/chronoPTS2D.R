#####	chronoPTS2D	#####

chronoPTS2D <- function(x, y, tree, tip.age=NULL, min.age=NULL, radius=NULL, xlab=NULL, ylab=NULL, zlab=NULL, col=NULL, clade=NULL, A=NULL, method=c("REML","ML","PIC","GLS"), node.label=T, node.adj=NULL, node.cex=NULL, tip.label="num", tip.adj=NULL, tip.cex=NULL, box=TRUE, shadow=FALSE){
  require(ape)
	require(rgl)
	if(is.null(col)){col=rep("black",length(tree$tip))}
	if(is.null(radius)){radius=0.2}
	if(is.null(xlab)){xlab="X"}
	if(is.null(ylab)){ylab="Y"}
	if(is.null(zlab)){zlab="age"}
	tip.age <- nodeAges(tree, tip.age, min.age)$tip.age
	nodeage <- nodeAges(tree, tip.age, min.age)$nodeage
	if(is.null(A)){
	  if(method=="REML"){
	    reml.x <- ace(x,tree,method="REML")
	    reml.y <- ace(y,tree,method="REML")
	    ace.x <- reml.x$ace
	    ace.y <- reml.y$ace
	  }
		if(method=="ML"){
			ml.x <- ace(x,tree,method="ML")
			ml.y <- ace(y,tree,method="ML")
			ace.x <- ml.x$ace
			ace.y <- ml.y$ace
		}
		if(method=="PIC"){
			pic.x <- ace(x,tree,method="pic")
			pic.y <- ace(y,tree,method="pic")
			ace.x <- pic.x$ace
			ace.y <- pic.y$ace
		}
		if(method=="GLS"){
		  gls.x <- ace(x,tree,method="GLS")
		  gls.y <- ace(y,tree,method="GLS")
		  ace.x <- gls.x$ace
		  ace.y <- gls.y$ace
		}
			}else{ace.x<-A[,1];ace.y<-A[,2]}
	if(length(col)==1){col=rep(col,length(tree$tip))}else{col=col}
	if(is.null(clade)){nodeCol <- rep("gray", tree$Nnode)
		node.col <- cbind(seq(1:tree$Nnode),nodeCol)}else{
		node.col <- cladeCol(tree,clade,col)}
	age <- tip.age
	allnodecol <- c(col,as.character(node.col[,2]))
	X <- c(x,ace.x)
	names(X) <- c(1:length(X))
	# names(X)[1:length(x)] <- c(1:length(x))
	Y <- c(y,ace.y)
	names(Y) <- c(1:length(Y))
	# names(Y)[1:length(y)] <- c(1:length(y))
	ages <- c(age,nodeage)
	names(ages)[1:length(age)] <- c(1:length(age))
	col3d <- c(col,node.col[,2])
	names(col3d)[1:length(col)]<-c(1:length(col))
	open3d()
	plot3d(x,y,-age,col=col, type="s",radius=radius, xlab="", ylab="", zlab="", xlim=c(min(X), max(X)), ylim=c(min(Y), max(Y)), zlim=c(min(-ages), max(-ages)), box=F,axes=FALSE)
	spheres3d(ace.x, ace.y, -nodeage, radius=radius/2, col=node.col[,2],axes=FALSE)
	if(box==FALSE){axes3d(zat=-pretty(ages),zlab=pretty(ages))
		decorate3d(xlab=xlab, ylab=ylab, zlab=zlab, axes=FALSE)}else{axes3d(zat=-pretty(ages),zlab=pretty(ages),box=T)
		decorate3d(xlab=xlab, ylab=ylab, zlab=zlab, axes=FALSE)}
	nodes.x <-(X)
	nodes.y <-(Y)
	nodes.z <-(-ages)
	connectBranch3D(tree, nodes.x, nodes.y, nodes.z, allnodecol,alpha=NULL)
	if(node.label==T){
	if(is.null(node.adj)){node.adj=1.5}
	if(is.null(node.cex)){node.cex=0.5}
		labels <- names(ace.x)
		text3d(ace.x,ace.y,-nodeage,col=node.col[,2],text=labels,adj=node.adj,cex=node.cex)
		}
	if(tip.label=="num"){
		if(is.null(tip.adj)){tip.adj=1}
		if(is.null(tip.cex)){tip.cex=0.6}
			text3d(x, y, -age, col=col, text=seq(1:length(x)), adj=tip.adj, cex=tip.cex)
			}
	if(tip.label=="tip"){
		if(is.null(tip.adj)){tip.adj=1}
		if(is.null(tip.cex)){tip.cex=0.6}
			text3d(x, y, -age, col=col, text=tree$tip, adj=tip.adj, cex=tip.cex)
		}
	if(shadow){
		par3d(ignoreExtent=TRUE)
		bbox <- par3d("bbox")
		points3d(x, y, bbox[5], col=col, point_antialias=T, alpha=shadow)
		points3d(ace.x,ace.y, bbox[5], col=node.col[,2], point_antialias=T, alpha=shadow)
		nodes.z <- rep(bbox[5], length(ages))
		names(nodes.z) <- c(1:length(nodes.z))
		connectBranch3D(tree, nodes.x, nodes.y, nodes.z, allnodecol,alpha=shadow)
		}
	}


#####	Functions by Manabu Sakamoto (m.sakamoto@bristol.ac.uk)