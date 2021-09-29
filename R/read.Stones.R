read.Stones <- function(file){
  x <- readLines(file)
  x <- x[length(x)]
  x <- unlist(strsplit(x, ":"))[2]
  x <- as.numeric(x)
  return(x)
}