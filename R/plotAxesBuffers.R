plotAxesBuffers <- function(x, y, rev.x=NULL, ...){
  ax.buff <- axesBuffers(x = x, y = y)
  .xlim <- ax.buff$xlim
  .ylim <- ax.buff$ylim
  if(rev.x) .xlim <- rev(.xlim)
  plot(.xlim, .ylim, xlim = .xlim, ylim = .ylim, type = "n", xaxs = "i", yaxs = "i", axes=F, ...)
  axis(1, at=.xlim, labels=c("",""), lwd.ticks=0)
  axis(1, lwd=0, lwd.ticks=1)
  axis(2, at=.ylim, labels=c("",""), lwd.ticks=0)
  axis(2, lwd=0, lwd.ticks=1, las=2)
}