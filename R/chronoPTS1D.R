#####	Function chronoPTS1D	#####
chronoPTS1D<-function(x, tree, tip.age=NULL, min.age=NULL, xlab=NULL, ylab=NULL, col=NULL, clade=NULL,  A=NULL, method="REML", node.label=T, node.adj=NULL, node.cex=NULL, tip.label="num", tip.adj=NULL, tip.cex=NULL, CI=FALSE, pct.mar=0.05, ...){
  require(ape)
  #require(gplots)
  if(is.null(col)){col=rep("black",length(tree$tip))}
  if(is.null(xlab)){xlab="age"}
  if(is.null(ylab)){ylab="x"}
  tip.age <- nodeAges(tree, tip.age, min.age)$tip.age
  nodeage <- nodeAges(tree, tip.age, min.age)$nodeage
  if(is.null(A)){
    if(method=="REML"){
      ml.x <- ace(x,tree,method="REML",CI=T)
      ace.x <- ml.x$ace
      ci.x <- ml.x$CI95
    }
    if(method=="ML"){
      ml.x <- ace(x,tree,method="ML",CI=T)
      ace.x <- ml.x$ace
      ci.x <- ml.x$CI95
    }
    if(method=="PIC"){
      pic.x <- ace(x,tree,method="pic",CI=T)
      ace.x <- pic.x$ace
      ci.x <- pic.x$CI95
    }
    if(method=="GLS"){
      ml.x <- ace(x,tree,method="GLS",CI=T)
      ace.x <- ml.x$ace
      ci.x <- ml.x$CI95
    }
  }else{
    A <- as.matrix(A)
    if(length(A[1,])>1){ace.x <- A[,1]; ci.x <- A[,2:3]}else{
      ace.x<-as.numeric(A);names(ace.x)<-rownames(A)
    }
  }
  if(length(col)==1){col=rep(col,length(tree$tip))}else{col=col}
  if(is.null(clade)){nodeCol <- rep("gray", tree$Nnode)
  node.col <- cbind(seq(1:tree$Nnode),nodeCol)}else{
    node.col <- cladeCol(tree,clade,col)}
  allnodecol <- c(col,as.character(node.col[,2]))
  age <- as.numeric(tip.age)
  x <- as.numeric(x)
  if(tip.label=="tip"){padding <- ((max(nodeage)-min(nodeage))/length(pretty(c(min(age), max(nodeage)))))}else{padding <- (max(nodeage)-min(nodeage))/25}
  if(CI==TRUE){
    #upp <- ci.x[,2] - ace.x
    #lwr <- ace.x - ci.x[,1]
    #rx <- cbind(range(x),range(ci.x),range(ace.x))
    upp <- ci.x[,2]
    lwr <- ci.x[,1]
    #plotCI(nodeage, ace.x, uiw=upp, liw=lwr,cex=0.5, col="grey90", xlim=rev(c( min(age)-padding, max(nodeage) )), ylim=c(min(rx),max(rx)), xlab=xlab, ylab=ylab, gap=0, xaxt="n", yaxt="n")
    plot(
      nodeage, ace.x, cex=0.5, col="grey90", 
      xlim=rev(range(c( {min(age)-padding}, {max(nodeage) + diff(range(age, nodeage))*pct.mar} ))), 
      # ylim=range(pretty(c(min(c(x, ace.x)) - diff(range(c(x, ace.x)))*0.1, max(c(x, ace.x))))),
      ylim=range(c({min(c(x, lwr)) - diff(range(c(x, upp, lwr)))*pct.mar}, {max(c(x, upp)) + diff(range(c(x, lwr, upp)))*pct.mar})),
      xlab=xlab, ylab=ylab, axes=F, xaxs="i", yaxs="i", ...
    )
    arrows(nodeage, ace.x, nodeage, lwr, length=0.05, angle=90, code=3, lwd=0.5, col="grey")
    arrows(nodeage, ace.x, nodeage, upp, length=0.05, angle=90, code=3, lwd=0.5, col="grey")
    axis(1, at=c(min(age),{max(nodeage) + diff(range(age, nodeage))*0.1}), labels=c("",""), lwd.ticks=0)
    axis(1, lwd=0, lwd.ticks=1)
    axis(2, at=range(c({min(c(x, lwr)) - diff(range(c(x, upp, lwr)))*pct.mar}, {max(c(x, upp)) + diff(range(c(x, lwr, upp)))*pct.mar})), labels=c("",""), lwd.ticks=0)
    axis(2, lwd=0, lwd.ticks=1, las=2)
  }else{
    plot(
      nodeage, ace.x, cex=0.5, col="grey90", 
      xlim=rev(range(c( {min(age)-padding}, {max(nodeage) + diff(range(age, nodeage))*pct.mar} ))),
      # ylim=range(pretty(c(min(c(x, ace.x)) - diff(range(c(x, ace.x)))*0.1, max(c(x, ace.x))))),
      ylim=range(c({min(c(x, ace.x)) - diff(range(c(x, ace.x)))*pct.mar}, {max(c(x, ace.x)) + diff(range(c(x, ace.x)))*pct.mar})),
      xlab=xlab, ylab=ylab, axes=F, xaxs="i", yaxs="i", ...
    )
    axis(1, at=c(min(age),{max(nodeage) + diff(range(age, nodeage))*pct.mar}), labels=c("",""), lwd.ticks=0)
    axis(1, lwd=0, lwd.ticks=1)
    axis(2, at=range(c({min(c(x, ace.x)) - diff(range(c(x, ace.x)))*pct.mar}, {max(c(x, ace.x)) + diff(range(c(x, ace.x)))*pct.mar})), labels=c("",""), lwd.ticks=0)
    axis(2, lwd=0, lwd.ticks=1, las=2)
  }
  nodes.x <- data.frame(c(age,nodeage))
  nodes.y <- data.frame(c(x,ace.x))
  points(nodeage, ace.x, col="black", bg=node.col[,2], pch=21, cex=0.5)
  connectBranch2D(tree, nodes.x, nodes.y, col=allnodecol)
  points(age, x, pch=21, bg=col, col="black")
  #points(age, x)
  if(node.label==T){
    if(is.null(node.adj)){node.adj=1.5}
    if(is.null(node.cex)){node.cex=0.5}
    labels <- names(ace.x)
    text(nodeage, ace.x, col=node.col[,2], label=labels, adj=node.adj, cex=node.cex)
  }
  if(tip.label=="num"){
    if(is.null(tip.adj)){tip.adj=c(-1,1)}
    if(is.null(tip.cex)){tip.cex=0.6}
    text(age, x, col=col, label=seq(1:length(x)), adj=tip.adj, cex=tip.cex)
  }
  if(tip.label=="tip"){
    if(is.null(tip.adj)){tip.adj=c(-0.1,1)}
    if(is.null(tip.cex)){tip.cex=0.5}
    text(age, x, col=col, label=tree$tip, adj=tip.adj, cex=tip.cex, font=3)
  }
}



#####	Functions by Manabu Sakamoto (m.sakamoto@bristol.ac.uk)