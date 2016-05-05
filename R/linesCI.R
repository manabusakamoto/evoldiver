#####	Compute and plot confidence intervals on regression:
linesCI <-function(x, level=0.95, col=NULL, lty=NULL, ...){
  if(is.null(col)){col="black"}
  if(is.null(lty)){lty=2}
  p <- 1 - {{1-level}/2}
  xm <- sapply(x$model[2], mean)
  n <- nrow(x$model[2])
  ssx <- sum(x$model[2]^2)-sum(x$model[2])^2/n
  s.t <- qt(p,(n-2))
  xv <- seq(min(x$model[2]),max(x$model[2]),(max(x$model[2])-min(x$model[2]))/100)
  yv <- coef(x)[1]+coef(x)[2]*xv
  se <- sqrt(summary(x)$sigma^2*(1/n+(xv-xm)^2/ssx))
  ci <- s.t*se
  uyv <- yv+ci
  lyv <- yv-ci
  lines(xv, uyv, lty=lty, col=col, ...)
  lines(xv, lyv, lty=lty, col=col, ...)
}

# #####	Compute and plot confidence intervals on regression:
# linesCI <-function(x,col=NULL){
# 	if(is.null(col)){col="black"}
# 	xm<-sapply(x$model[2], mean)
# 	n<-length(x$model[[2]])
# 	ssx<-sum(x$model[2]^2)-sum(x$model[2])^2/n
# 	s.t<-qt(0.975,(n-2))
# 	xv<-seq(min(x$model[2]),max(x$model[2]),(max(x$model[2])-min(x$model[2]))/100)
# 	yv<-coef(x)[1]+coef(x)[2]*xv
# 	se<-sqrt(summary(x)$sigma^2*(1/n+(xv-xm)^2/ssx))
# 	ci<-s.t*se
# 	uyv<-yv+ci
# 	lyv<-yv-ci
# 	lines(xv,uyv,lty=2, col=col)
# 	lines(xv,lyv,lty=2, col=col)
# }