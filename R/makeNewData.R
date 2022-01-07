makeNewData <- function(data, postMCMC, EffectsMain){
  
  vnms <- names(data)[-c(1,2)]
  if(!is.data.table(postMCMC)) postMCMC <- data.table(postMCMC)
  if(sum(grepl("^Alpha", names(postMCMC)) | grepl("^Beta", names(postMCMC)))>0){
    cf.dt <- postMCMC[, grepl("^Alpha", names(postMCMC)) | grepl("^Beta", names(postMCMC)), with=F]
    setnames(cf.dt, c("Intercept", vnms))
  }else{
    cf.dt <- postMCMC[, is.element(names(postMCMC), vnms), with=F]
  }
  
  
  if(length(EffectsMain) == 1){
    eff.nms <- names(cf.dt)[grepl(EffectsMain, names(cf.dt))]
  }else{
    eff.nms <- EffectsMain
  }
  
  dt.new <- data.table()
  for(i in 1:length(eff.nms)){
    # i <- 1
    .eff.nm <- eff.nms[i]
    .v <- unlist(data[, .eff.nm, with=F])
    .v.min <- min(.v)
    .v.max <- max(.v)
    .v.seq <- seq(.v.min, .v.max, length.out = 100)
    dt.new <- data.table(dt.new, .v.seq)
  }
  setnames(dt.new, eff.nms)
  
  # eff.fix <- names(cf.dt)[!is.element(eff.nms, names(cf.dt))]
  eff.fix <- names(cf.dt)[!is.element(names(cf.dt), eff.nms)]
  dt.fix <- data[, eff.fix, with=F]
  dt.median <- dt.fix[, lapply(.SD, median)]
  dt.new <- data.table(dt.new, dt.median)
  dt.new <- dt.new[, names(cf.dt), with=F]
  
  newData <- list(Data=data, newData=dt.new, EffectsMain=dt.new[, eff.nms, with=F])
  return(newData)
}
