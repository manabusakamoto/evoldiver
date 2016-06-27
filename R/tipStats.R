
tipStats <- function(x, time, FAD, LAD){
  stats <- matrix(nrow=length(FAD), ncol=2)
  cond0 <- class(time)=="matrix"|class(time)=="data.frame"
  if(!cond0){
    time <- cbind(time, c(time[-1],0))
  }
  time1 <- time[,1]
  time2 <- time[,2]
  for(i in 1:length(FAD)){
    age.1 <- round(FAD[i],2)
    if(age.1 > max(time1)) age.1 <- max(time1)
    age.2 <- round(LAD[i],2)
    .fad <- .lad <- vector(length=nrow(time))
    for(j in 1:nrow(time)){
      .t <- time[j,]
      .fad[j] <- age.1 <= .t[1] & age.1 > .t[2]
      if(j!=nrow(time)){
        .lad[j] <- age.2 <= .t[1] & age.2 > .t[2]
      }else{
        .lad[j] <- age.2 <= .t[1] & age.2 >= .t[2]
      }
    }
    id1 <- which(.fad)
    id2 <- which(.lad)
    id <- id1:id2
    term <- x[id]
    term.length <- max(c(time1[id],time2[id]))-min(c(time1[id],time2[id]))
    if(cond0){
      dur <- time1-time2
      dur1 <- time1[id1] - age.1
      dur2 <- time1[id2] - age.2
      dur[id1] <- dur1
      dur[id2] <- dur2
      mean.term <- sum(term*dur[id])/term.length
      if(length(term)==1){
        var.term <- 0
      }else{
        var.term <- 1/{length(term)-1}*sum({term-mean.term}^2)
      }
    }else{
      mean.term <- mean(term)
      var.term <- var(term) 
    }
    stats[i,1] <- mean.term
    stats[i,2] <- var.term
  }
  stats[is.na(stats)]<-0
  stats <- as.data.frame(stats)
  names(stats) <- c("mean", "var")
  return(stats)
}

