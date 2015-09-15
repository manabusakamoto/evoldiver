##### Function PTS2D
PTS2D<-function(x,y,tree,xlab=NULL,ylab=NULL,col=NULL,clade=NULL,A=NULL,method=c("REML","ML","PIC"),node.label=F, tip.label="num",...){
	require(ape)
	if(is.null(col)){col=rep("black",length(tree$tip))}
	if(is.null(xlab)){xlab="X"}
	if(is.null(ylab)){ylab="Y"}
	if(is.null(A)){
	  if(method=="REML"){
	    reml.x <- ace(x,tree,method="REML")
	    reml.y <- ace(y,tree,method="REML")
	    ace.x <- reml.x$ace
	    ace.y <- reml.y$ace}
		if(method=="ML"){
			ml.x <- ace(x,tree,method="ML")
			ml.y <- ace(y,tree,method="ML")
			ace.x <- ml.x$ace
			ace.y <- ml.y$ace}
		if(method=="PIC"){
			pic.x <- ace(x,tree,method="pic")
			pic.y <- ace(y,tree,method="pic")
			ace.x <- pic.x$ace
			ace.y <- pic.y$ace}
		}else{ace.x<-A[,1];ace.y<-A[,2]}
	if(length(col)==1){col=rep(col,length(tree$tip))}else{col=col}
	if(is.null(clade)){nodeCol <- rep("gray", tree$Nnode)
		node.col <- cbind(seq(1:tree$Nnode),nodeCol)}else{
		node.col <- cladeCol(tree,clade,col)}
		allnodecol <- c(col,as.character(node.col[,2]))
		plot(x, y, xlim=c(min(x), max(x)), ylim=c(min(y), max(y)), xlab=xlab, ylab=ylab, axes=F,...)
		box()
		axis(1)
		axis(2,las=2)
		nodes.x <- data.frame(c(as.numeric(x),ace.x))
		nodes.y <- data.frame(c(as.numeric(y),ace.y))
		points(ace.x, ace.y, col=node.col[,2], pch=19, cex=0.5)
		connectBranch2D(tree, nodes.x, nodes.y, col=allnodecol)
		points(x, y,pch=19,col=col,...)
		points(x, y,...)
		if(node.label==T){
			text(ace.x,ace.y,label=(length(x)+1):(length(x)+length(ace.x)),	cex=0.5, adj=c(1,1), col=node.col[,2],...)
			}
		if(tip.label=="num"){
			text(x, y, label=seq(1:length(x)), adj=c(1.5,1.5), cex=0.5, col=col,...)
			}
		if(tip.label=="tip"){
			text(x, y, label=tree$tip.label, adj=c(1,1), cex=0.5, font=3, col=col,...)
			}
}



#####	Functions by Manabu Sakamoto (m.sakamoto@bristol.ac.uk)