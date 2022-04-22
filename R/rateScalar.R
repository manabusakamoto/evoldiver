rateScalar <- function(phy, phy.ref){
  if(length(phy) > 1){
    function()
    r <- lapply(phy, rateScalar.internal, phy.ref = phy.ref)
    r <- lapply(r, function(x){data.table(t(x))})
    r <- rbindlist(r)
    r.med <- unlist(r[, lapply(.SD, median)])
    r.mean <- unlist(r[, lapply(.SD, mean)])
    rates <- list(RateScalars = r, RateScalars_Median=r.med, RateScalars_Mean=r.mean)
  }else{
    rates <- rateScalar.internal(phy, phy.ref)
  }
  return(rates)
}