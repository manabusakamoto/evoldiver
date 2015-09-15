# create BayesPhylogenies command lines
cmdBayesPhy <- function(part.name, part.type, it, cores, printfreq, output, clock=NULL, ordchar=NULL, tcal=NULL){
  head <- c("BEGIN BayesPhylogenies",
            paste("\tCreatePart\t", part.name, "\t", part.type, sep=""),
            paste("\tit\t", it,sep=""),
            paste("\t#\tcores\t", cores, sep=""),
            paste("\tprintfreq\t", printfreq, sep=""),
            paste("\toutput\t", output, sep="")
            )
  if(!is.null(clock)) head <- c(head, paste("\tclock\t", clock, sep=""))
  #tail <- c("\n", "END;")
  tail <- "END;"
  out <- c(head, ordchar, tcal, tail)
  return(out)
}