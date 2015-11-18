read.BayesTraits <- function(path, data.table=NULL){
  if(!is.null(data.table))require(data.table)
  x <- readLines(path)
  rm(path)
  id <- grep("Iteration\tLh", x)
  x <- x[-c(1:{id-1})]
  id <- grep("Sec:", x)
  if(length(id)>0){x <- x[1:{id-1}]}
  dd <- x[1]
  strsplit(dd, "\t")
  dt <- sapply(x, strsplit, split="\t")
  names(dt) <- NULL
  cnm <- dt[[1]]
  dt <- lapply(dt[-1], as.numeric, na.omit)
  if(!is.null(data.table)){
    dt <- data.table(do.call("rbind", dt))
    setnames(dt, cnm)
  }else{
    dt <- data.frame(do.call("rbind", dt))
    setNames(dt, cnm)
  }
  return(dt)
}