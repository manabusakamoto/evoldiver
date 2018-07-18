coeff.BT <- function(x, data=NULL, header=NULL){
  # x is BayesTraits log file object
  # data is input data object with headers
  # header is vector of variable names
  if(exists("data") & exists("header")) {
    warning("Arguments data and header are specified: Taking header argument")
    v.dt <- header
  }else{
    if(!is.null(data) & is.null(header)){
      v.dt <- names(data)
      v.dt <- v.dt[-1]
      v.dt[1] <- "Intercept"
    }
    if(!is.null(header) & is.null(data)) v.dt <- header
  }
  v.lg <- names(lg)
  v.lg <- v.lg[grepl("^Alpha", v.lg) | grepl("^Beta", v.lg)]
  lg.dm <- lg[, v.lg, with=F]
  if(exists("v.dt")) setnames(lg.dm, v.dt)
  return(lg.dm)
}