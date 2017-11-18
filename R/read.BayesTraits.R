read.BayesTraits <- function(path, data.table=NULL){
  if(!is.null(data.table))require(data.table)
  x <- readLines(path)
  rm(path)
  id <- grep("Iteration\tLh", x)
  id <- max(id)
  x <- x[-c(1:{id-1})]
  id <- grep("Sec:", x)
  if(length(id)>0){
    id <- id[1]
    x <- x[1:{id-1}]
  }
  dd <- x[1]
  dt <- sapply(x, strsplit, split="\t")
  names(dt) <- NULL
  cnm <- dt[[1]]
  # dt <- lapply(dt[-1], as.numeric, na.omit)
  dt <- dt[-1]
  if(!is.null(data.table)){
    dt <- data.table(do.call("rbind", dt))
    setnames(dt, cnm)
  }else{
    dt <- data.frame(do.call("rbind", dt))
    names(dt) <- cnm
  }
  dm <- vector(mode = "list", length = ncol(dt))
  for(i in 1: ncol(dt)){
    # i <- 6
    y <- dt[[i]]
    y2 <- try(as.numeric(y), silent = T)
    if(sum(is.na(y2))==length(y2)) y2 <- y
    dm[[i]] <- y2
  }
  
  if (!is.null(data.table)) {
    df <- as.data.table(dm)
    setnames(df, cnm)
  }else{
    df <- as.data.frame(dm)
    df <- setNames(df, cnm)
  }
  # return(dt)
  return(df)
}
