plotAxesBuffers <- function(x, y, ...){
  ax.buff <- axesBuffers(x = x, y = y)
  .xlim <- ax.buff$xlim
  .ylim <- ax.buff$ylim
  plot(.xlim, .ylim, xlim = .xlim, ylim = .ylim, type = "n", xlab = "Time", ylab = "Net speciation per myr", xaxs = "i", yaxs = "i", axes=F)
  axis(1, at=.xlim, labels=c("",""), lwd.ticks=0)
  axis(1, lwd=0, lwd.ticks=1)
  axis(2, at=.ylim, labels=c("",""), lwd.ticks=0)
  axis(2, lwd=0, lwd.ticks=1, las=2)
}