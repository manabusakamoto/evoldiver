coeff.BT <- function(dt, lg){
  v.lg <- names(lg)
  v.dt <- names(dt)
  v.lg <- v.lg[grepl("^Alpha", v.lg) | grepl("^Beta", v.lg)]
  lg.dm <- lg[, v.lg, with=F]
  v.dt <- v.dt[-1]
  v.dt[1] <- "Intercept"
  setnames(lg.dm, v.dt)
  return(lg.dm)
}