\name{chronoPTS2D}
\alias{chronoPTS2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot 2D chronophylotraitspace plot
%%  ~~function to do ... ~~
}
\description{
Plot 2D traitspace across phylogeny and through time using ancestor reconstruction
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chronoPTS2D(x, y, tree, tip.age = NULL, min.age = NULL, radius = NULL, xlab = NULL, ylab = NULL, zlab = NULL, col = NULL, clade = NULL, A = NULL, method = c("REML", "ML", "PIC", "GLS"), node.label = T, node.adj = NULL, node.cex = NULL, tip.label = "num", tip.adj = NULL, tip.cex = NULL, box = TRUE, shadow = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a vector of the first trait/variable axis, e.g., PC1
%%     ~~Describe \code{x} here~~
}
  \item{y}{
  a vector of the second trait/variable axis, e.g., PC2
%%     ~~Describe \code{y} here~~
}
  \item{tree}{
  a time-scaled phylo object
%%     ~~Describe \code{tree} here~~
}
  \item{tip.age}{
  an optional vector of tip ages in the same order as tree$tip.label
%%     ~~Describe \code{tip.age} here~~
}
  \item{min.age}{
  an optional single numerical value for the minimum age of the tips. Alternative to tip.age
%%     ~~Describe \code{min.age} here~~
}
  \item{radius}{
  an optional numerical value to specify size of the 3D points in rgl
%%     ~~Describe \code{radius} here~~
}
  \item{xlab}{
  X label
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
  Y label
%%     ~~Describe \code{ylab} here~~
}
  \item{zlab}{
  Z label, defaults to "age"
%%     ~~Describe \code{zlab} here~~
}
  \item{col}{
  an optional vector of colours for data points
%%     ~~Describe \code{col} here~~
}
  \item{clade}{
  an optional vector of clade memberships, can be the same as col
%%     ~~Describe \code{clade} here~~
}
  \item{A}{
  an optional matrix of user-specified ancestor values, in the same order as the nodes in the phylo object
%%     ~~Describe \code{A} here~~
}
  \item{method}{
  if A is absent, then the method in which to reconstruct ancestor values. Choose from "REML" (default), "ML", or "PIC". The code for option "GLS" is incomplete.
%%     ~~Describe \code{method} here~~
}
  \item{node.label}{
  logical value indicating whether or not to display node labels
%%     ~~Describe \code{node.label} here~~
}
  \item{node.adj}{
  an optional numerical value to specify adjustment to node label position passed to function text3D. Default value=1.5
%%     ~~Describe \code{node.adj} here~~
}
  \item{node.cex}{
  an optional numerical value to specify the size of the node labels passed to function text3D. Default value=0.5
%%     ~~Describe \code{node.cex} here~~
}
  \item{tip.label}{
  argument to specify which type of tip label to display, either "tip" (tip labels), or "num" (numbers)
%%     ~~Describe \code{tip.label} here~~
}
  \item{tip.adj}{
  an optional numerical value to specify adjustment to tip label position passed to function text3D. Default value=1
%%     ~~Describe \code{tip.adj} here~~
}
  \item{tip.cex}{
  an optional numerical value to specify the size of the tip labels passed to function text3D. Default value=0.6
%%     ~~Describe \code{tip.cex} here~~
}
  \item{box}{
  logical argument to specify whether or not to display a box around the plot
%%     ~~Describe \code{box} here~~
}
  \item{shadow}{
  logical argument to specify whether or not to display a dropdown shadow (2D projection) of the traitspace at the bottom of the plot
%%     ~~Describe \code{shadow} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This funciton plots a 2D chronophylotraitspace (chronoPTS), or phylotraitspace (also known as phylomorphospace) through time. Ancestor values for each node of the tree are either reconstructed through the ace function in ape, or can be specified as a matrix or data.frame object of X-Y coordinates. A time-scaled phylogeny in the form of a phylo object must be supplied.
X and Y are two traits of interest, typically morphometric PC axes (PC1 and PC2) but can be any two variables including ecospace or functionspace axes.
If tip ages are supplied, the Z-axis is scaled accordingly based on tip ages and tree height. If tip ages are not supplied, the yougest tip is assumed to be ultrametric (age=0) and Z-axis scaled back in time according to tree height. If minimum age is supplied, then that value will be assigned to the youngest tip and Z-axis scaled according to tree height.
Tips points can be coloured according to the argument col, which should be a vector specifying colours (e.g., colour names) with the same length as tree$tip.label, with each tip assigned a colour. If the argument clade is given, either as clade memberships (clade names), or as colour names corresponding to col, then branches belonging to each monophyletic clade as identified from the phylo object will be coloured according to the tip colours of the members of those clades. To keep it simple, the argument clade can just be the same vector as argument col.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns an interactive rgl graphics device.
}
\references{
 Sakamoto M, Ruta M (2012) Convergence and Divergence in the Evolution of Cat Skulls: Temporal and Spatial Patterns of Morphological Diversity. PLoS ONE 7(7): e39752. doi:10.1371/journal.pone.0039752
http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0039752
%% ~put references to the literature/web site here ~
}
\author{
Manabu Sakamoto
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, tree, tip.age = NULL, min.age = NULL, radius = NULL, 
    xlab = NULL, ylab = NULL, zlab = NULL, col = NULL, clade = NULL, 
    A = NULL, method = c("REML", "ML", "PIC", "GLS"), node.label = T, 
    node.adj = NULL, node.cex = NULL, tip.label = "num", tip.adj = NULL, 
    tip.cex = NULL, box = TRUE, shadow = NULL) 
{
    require(ape)
    require(rgl)
    if (is.null(col)) {
        col = rep("black", length(tree$tip))
    }
    if (is.null(radius)) {
        radius = 0.2
    }
    if (is.null(xlab)) {
        xlab = "X"
    }
    if (is.null(ylab)) {
        ylab = "Y"
    }
    if (is.null(zlab)) {
        zlab = "age"
    }
    tip.age <- nodeAges(tree, tip.age, min.age)$tip.age
    nodeage <- nodeAges(tree, tip.age, min.age)$nodeage
    if (is.null(A)) {
        if (method == "REML") {
            reml.x <- ace(x, tree, method = "REML")
            reml.y <- ace(y, tree, method = "REML")
            ace.x <- reml.x$ace
            ace.y <- reml.y$ace
        }
        if (method == "ML") {
            ml.x <- ace(x, tree, method = "ML")
            ml.y <- ace(y, tree, method = "ML")
            ace.x <- ml.x$ace
            ace.y <- ml.y$ace
        }
        if (method == "PIC") {
            pic.x <- ace(x, tree, method = "pic")
            pic.y <- ace(y, tree, method = "pic")
            ace.x <- pic.x$ace
            ace.y <- pic.y$ace
        }
        if (method == "GLS") {
            gls.x <- ace(x, tree, method = "GLS")
            gls.y <- ace(y, tree, method = "GLS")
            ace.x <- gls.x$ace
            ace.y <- gls.y$ace
        }
    }
    else {
        ace.x <- A[, 1]
        ace.y <- A[, 2]
    }
    if (length(col) == 1) {
        col = rep(col, length(tree$tip))
    }
    else {
        col = col
    }
    if (is.null(clade)) {
        nodeCol <- rep("gray", tree$Nnode)
        node.col <- cbind(seq(1:tree$Nnode), nodeCol)
    }
    else {
        node.col <- cladeCol(tree, clade, col)
    }
    age <- tip.age
    allnodecol <- c(col, as.character(node.col[, 2]))
    X <- c(x, ace.x)
    names(X)[1:length(x)] <- c(1:length(x))
    Y <- c(y, ace.y)
    names(Y)[1:length(y)] <- c(1:length(y))
    ages <- c(age, nodeage)
    names(ages)[1:length(age)] <- c(1:length(age))
    col3d <- c(col, node.col[, 2])
    names(col3d)[1:length(col)] <- c(1:length(col))
    open3d()
    plot3d(x, y, -age, col = col, type = "s", radius = radius, 
        xlab = "", ylab = "", zlab = "", xlim = c(min(X), max(X)), 
        ylim = c(min(Y), max(Y)), zlim = c(min(-ages), max(-ages)), 
        box = F, axes = FALSE)
    spheres3d(ace.x, ace.y, -nodeage, radius = radius/2, col = node.col[, 
        2], axes = FALSE)
    if (box == FALSE) {
        axes3d(zat = -pretty(ages), zlab = pretty(ages))
        decorate3d(xlab = xlab, ylab = ylab, zlab = zlab, axes = FALSE)
    }
    else {
        axes3d(zat = -pretty(ages), zlab = pretty(ages), box = T)
        decorate3d(xlab = xlab, ylab = ylab, zlab = zlab, axes = FALSE)
    }
    nodes.x <- (X)
    nodes.y <- (Y)
    nodes.z <- (-ages)
    connectBranch3D(tree, nodes.x, nodes.y, nodes.z, allnodecol, 
        alpha = NULL)
    if (node.label == T) {
        if (is.null(node.adj)) {
            node.adj = 1.5
        }
        if (is.null(node.cex)) {
            node.cex = 0.5
        }
        labels <- names(ace.x)
        text3d(ace.x, ace.y, -nodeage, col = node.col[, 2], text = labels, 
            adj = node.adj, cex = node.cex)
    }
    if (tip.label == "num") {
        if (is.null(tip.adj)) {
            tip.adj = 1
        }
        if (is.null(tip.cex)) {
            tip.cex = 0.6
        }
        text3d(x, y, -age, col = col, text = seq(1:length(x)), 
            adj = tip.adj, cex = tip.cex)
    }
    if (tip.label == "tip") {
        if (is.null(tip.adj)) {
            tip.adj = 1
        }
        if (is.null(tip.cex)) {
            tip.cex = 0.6
        }
        text3d(x, y, -age, col = col, text = tree$tip, adj = tip.adj, 
            cex = tip.cex)
    }
    if (is.null(shadow) == FALSE) {
        par3d(ignoreExtent = TRUE)
        bbox <- par3d("bbox")
        points3d(x, y, bbox[5], col = col, point_antialias = T, 
            alpha = shadow)
        points3d(ace.x, ace.y, bbox[5], col = node.col[, 2], 
            point_antialias = T, alpha = shadow)
        nodes.z <- rep(bbox[5], length(ages))
        names(nodes.z) <- c(1:length(nodes.z))
        connectBranch3D(tree, nodes.x, nodes.y, nodes.z, allnodecol, 
            alpha = shadow)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
