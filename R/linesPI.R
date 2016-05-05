#####	Compute and plot prediction intervals on regression:
linesPI <-function(x, level=0.95, col=NULL, lty=NULL, ...){
  if(is.null(col)){col="black"}
  if(is.null(lty)){lty=3}
  p <- 1 - {{1-level}/2}
  xm <- sapply(x$model[2], mean)
  n <- nrow(x$model[2])
  ssx <- sum(x$model[2]^2)-sum(x$model[2])^2/n
  s.t <- qt(p,(n-2))
  xv <- seq(min(x$model[2]),max(x$model[2]),(max(x$model[2])-min(x$model[2]))/100)
  yv <- coef(x)[1]+coef(x)[2]*xv
  se <- sqrt(summary(x)$sigma^2*(1+{1/n}+{(xv-xm)^2/ssx}))
  ci <- s.t*se
  uyv <- yv+ci
  lyv <- yv-ci
  lines(xv,uyv,lty=lty, col=col, ...)
  lines(xv,lyv,lty=lty, col=col, ...)
}
# linesPI <- function(x, col=NULL, lty=NULL, level=NULL){
#   if(is.null(level)){level=0.95}
#   if(is.null(col)){col="black"}
#   if(is.null(lty)){lty=3}
#   nx <- x$model[,2]
#   nx <- seq(min(nx), max(nx), length.out = 100)
#   ndata <- data.frame(nx)
#   names(ndata) <- names(x$model)[2]
#   pred <- data.frame(predict(x, ndata, level=level, interval="prediction"))
#   lwr <- pred$lwr
#   upr <- pred$upr
#   lines(nx, lwr, col=col, lty=lty)
#   lines(nx, upr, col=col, lty=lty)
# }