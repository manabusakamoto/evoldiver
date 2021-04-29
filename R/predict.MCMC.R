predict.MCMC <- function(newData, postMCMC, average=NULL){
  if(!is.data.table(postMCMC)) postMCMC <- data.table(postMCMC)
  if(is.null(average)) average <- "median"
  newData <- newData$newData
  vnms <- names(newData)
  if(sum(grepl("^Alpha", names(postMCMC)) | grepl("^Beta", names(postMCMC)))>0){
    cf.dt <- postMCMC[, grepl("^Alpha", names(postMCMC)) | grepl("^Beta", names(postMCMC)), with=F]
    setnames(cf.dt, c("Intercept", vnms))
  }else{
    cf.dt <- postMCMC[, is.element(names(postMCMC), vnms), with=F]
  }
  pred <- matrix(nrow = nrow(cf.dt), ncol = nrow(newData))
  for(i in 1:nrow(cf.dt)){
    # i <- 1
    .cf <- unlist(cf.dt[i])
    if(sum(grepl("Intercept", names(.cf))) > 0){
      .a <- .cf[1]
      .b <- .cf[-1]
    }else{
      .a <- 0
      .b <- .cf
    }
    pred[i,] <- colSums(.b*t(newData)) + .a
  }
  return(Predictions = pred)
}