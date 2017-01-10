
##### function to delete TNT file formating
del.tnt.fmt <- function(x){
  # identify and delete TNT format preambles
  xread <- grep("xread", x, ignore.case=T)
  if(length(xread)>0){
    num <- grep("[[:digit:]]", x)[1]
    if( num == xread+1) x <- x[-num]
    x <- x[-xread]
  }
  nstates <- grep("nstates", x, ignore.case=T)
  if(length(nstates)>0){
    x <- x[-nstates]
  }
  code <- grep("&", x)
  if(length(code)>0){
    x[-code]
  }
  semicol <- grep(";", x)
  x <- x[-semicol]
  return(x)
}

################################################################################################################################


# function to convert NEXUS/TNT format data matrices to data.frame objects
phylo2df <- function(x){
  if(length(grep("xread", x, ignore.case=T))>0){
    x <- del.tnt.fmt(x)
  }
  if(length(grep("nexus", x, ignore.case=T))>0){
    if(sum(grepl("^matrix$", x, ignore.case=T))>0) .id <- grep("^matrix$", x, ignore.case=T)
    if(sum(grepl("^[[:space:]]matrix$", x, ignore.case=T))>0) .id <- grep("^[[:space:]]matrix$", x, ignore.case=T)
    x <- x[-c(1:.id)]
    x <- x[-c(grep(";",x)[1]:length(x))]
  }
  if(length(grep("^$",x))>0){
    x <- x[-grep("^$",x)]
  }
  pat <- "([[:digit:]])([[:space:]])([[:digit:]]){1,}"
  while(sum(grepl(pat, x)) > 0) x <- gsub(pat, "\\1\\3", x)
  x <- gsub("[[:space:]]{1,}", ";", x)
  x <- gsub("^;", "", x)
  x <- strsplit(x, ";")
  # x <- strsplit(x," {1,}| {2,}|\t")
  tx <- character(length(x))
  chm <- NULL
  for(i in 1:length(x)){
    xi <- x[[i]]
    tx[i] <- xi[1]
    ch <- xi[2]
    ch <- gsub(" ", "", ch)
    if(length(grep("[[:digit:]]",ch))>0){
      if(length(grep("\\(", ch))>0 | length(grep("\\[", ch))>0 | length(grep("\\{", ch))>0) {
        # ch <- unlist(strsplit(ch, "(?<!\\(|\\{|\\[|^)(?!\\)|\\}|\\]|\\d+\\}|\\d+\\)|\\d+\\]|$)", perl=T))
        ch <- unlist(strsplit(ch, "\\([^)]*\\)(*SKIP)(*F)|(?=)", perl=T))
      }
      #       if(length(grep("\\(", ch))>0) {ch <- unlist(strsplit(ch, "(?<!\\(|^)(?!\\)|\\d+\\)|$)", perl=T))}
      #       if(length(grep("\\[", ch))>0) {ch <- unlist(strsplit(ch, "(?<!\\[|^)(?!\\]|\\d+\\]|$)", perl=T))}
      #       if(length(grep("\\{", ch))>0) {ch <- unlist(strsplit(ch, "(?<!\\{|^)(?!\\}|\\d+\\}|$)", perl=T))}
      if(length(grep("\\(|\\[|\\{", ch))==0) {ch <- unlist(strsplit(ch, ""))}
    }else{
      ch <- unlist(strsplit(ch, ""))
    }
    # cat(i, length(unlist(ch)), "\n")
    chm <- rbind(chm, unlist(ch))
  }
  df <- data.frame(Taxon=tx, chm, stringsAsFactors=F)
  return(df)
}
# phylo2df <- function(x){
#   if(length(grep("xread", x, ignore.case=T))>0){
#     x <- del.tnt.fmt(x)
#   }
#   if(length(grep("nexus", x, ignore.case=T))>0){
#     x <- x[-c(1:grep("^matrix$", x, ignore.case=T))]
#     x <- x[-grep(";",x)]
#   }
#   if(length(grep("^$",x))>0){
#     x <- x[-grep("^$",x)]
#   }
#   x <- sub(" |\t", ";", x)
#   x <- strsplit(x, ";")
#   # x <- strsplit(x," {1,}| {2,}|\t")
#   tx <- character(length(x))
#   chm <- NULL
#   for(i in 1:length(x)){
#     xi <- x[[i]]
#     tx[i] <- xi[1]
#     ch <- xi[2]
#     ch <- gsub(" ", "", ch)
#     if(length(grep("[[:digit:]]",ch))>0){
#       if(length(grep("\\(", ch))>0) {ch <- strsplit(ch, "(?<!\\(|^)(?!\\)|\\d+\\)|$)", perl=T)}
#       if(length(grep("\\[", ch))>0) {ch <- strsplit(ch, "(?<!\\[|^)(?!\\]|\\d+\\]|$)", perl=T)}
#       if(length(grep("\\{", ch))>0) {ch <- strsplit(ch, "(?<!\\{|^)(?!\\}|\\d+\\}|$)", perl=T)}
#       if(length(grep("\\(|\\[|\\{", ch))==0) {ch <- strsplit(ch, "")}
#     }else{
#       ch <- strsplit(ch, "")
#     }
#     # cat(i, length(unlist(ch)), "\n")
#     chm <- rbind(chm, unlist(ch))
#   }
#   df <- data.frame(Taxon=tx, chm, stringsAsFactors=F)
#   return(df)
# }
################################################################################################################################


##### function to dummy code multi-state and polymorphic character states from data.frame object
dummycode <- function(df){
  # rename colnames
  names(df)[-1] <- gsub("[[:alpha:]]", "Ch", names(df)[-1])
  # dummy code polymorphic characters
  dt <- data.frame(Taxon=df[,1], stringsAsFactors=F)
  for(i in 2:ncol(df)){
    # Extract the ith character
    df.i <- data.frame(ch=gsub("\\?","NA",df[,i]),stringsAsFactors=F)
    # keep the character name
    ch.nm <- names(df)[i]
    # duplicate character states as a vector
    ch.i <- df.i$ch
    # code character states as dummy variables
    mm.i <- data.frame(model.matrix(~.-1, data=df.i))
    # identify missing state
    na <- grep("NA", names(mm.i))
    # remove column NA from dummy variable matrix
    if(length(na)>0){mm.i <- mm.i[-na]}
    # identify position of polymorphism by presence of a punctuation, typically a pair of parentheses
    n <- grep("[[:punct:]]",ch.i)
    # When character is polymorphic, then perform special dummy coding
    if(length(n)>0){
      # replace the punctuation with blank
      ch.i[n] <- gsub("[[:punct:]]", "", ch.i[n], "")
      # split polymorphic states into separate elements
      pm <- strsplit(ch.i[n],"")
      # vectorise the unique polymorphic states
      pst <- unique(unlist((pm)))
      # pattern to identify polymorphic characters from column names
      pat <- "^([[:alnum:]]+)[[:punct:]]([[:digit:]]+)[[:punct:]]$"
      # remember the position of polymorphic characters
      np <- grep("[[:punct:]]", names(mm.i))
      # only extract the polymorphic states from column names
      pp <- gsub(pat, "\\2", names(mm.i)[np])
      # rename dummy variable matrix columns
      names(mm.i) <- gsub("[[:punct:]]","",names(mm.i))
      # column names without any prefixes
      nm.i <- gsub("ch","",names(mm.i))
      # polymorphic states
      nm.p <- nm.i[np]
      # single states
      nm.s <- nm.i[-np]
      # extract polymorphic states from dummy matrix
      ch.p <- mm.i[np]
      # identify whether polymorphic states exist as single states
      cond <- is.element(pst, nm.s)
      # polymorphic states absent as single states
      sa <- pst[!cond]
      # extract single states from dummy matrix
      mat <- mm.i[-np]
      # record the number of columns of dummy matrix
      nc <- ncol(mat)
      # when there are polymorphic states that don't exist as single states
      if(length(sa)>0){
        # for each state absent as single states
        for(j in 1:length(sa)){
          # add a column of zeros to dummy matrix
          mat <- data.frame(mat, 0)
          # name the column according to the state
          names(mat)[nc+j] <- paste("ch", sa[j], sep="")
        } # end loop j
      } # end if statement where polymorphic states are absent as single-states
      # add polymorphic states to the appropriate single-state dummies
      for(j in 1:length(nm.p)){
        # separate each polymorphic state
        st.n <- paste("ch", unlist(strsplit(nm.p[j], "")), sep="")
        # index the corresponding single-state dummy variable
        el <- is.element(names(mat), st.n)
        # index the polymorphic state coded as 1
        cond <- ch.p[j]==1
        # code the corresponding single state as 1
        mat[cond,el] <- 1
      } # end loop j
      names(mat) <- gsub("ch", paste(ch.nm, "_", sep=""), names(mat))
    }else{
      # if no polymorphism exists, but character is multi-state then just simply clone the dummy matrix as mat
      if(ncol(mm.i)>2){
        mat <- mm.i
        names(mat) <- gsub("ch", paste(ch.nm, "_", sep=""), names(mat))
      }else{
        # if character is just single-state then clone the original character vector
        mat <- df.i
        names(mat) <- gsub("ch", ch.nm, names(mat))
      }# end if/else
    } # end if/else
    # rename mat columns with appropriate character name
    # replace all 0 characters with "NA"
    if(ncol(mat)>1){
      for(j in 1:nrow(mat)){
        if(sum(unlist(mat[j,]))==0){
          mat[j,] <- NA
        }
      } # end loop j
    }
    # add dummy matrix of ith character to the end of full dummy matrix
    dt <- data.frame(dt, mat)
  } # end loop i
  # replace non-0/1 binary states with 0/1 states
  for(i in 2:ncol(dt)){
    # convert character states to numeric and suppress warning about NAs being generated
    d <- suppressWarnings(as.numeric(dt[,i]))
    # omit the NA values
    d.n <- as.numeric(na.omit(d))
    # find locations of minimum values 
    d.min <- d==min(d.n)
    # find locations of maximum values (shouldn't be >0 if 0/1 binary)
    d.max <- d==max(d.n)
    # if the max value exceeds 1 then replace minimum value with 0 and max value with 1
    if(max(d.n)>1){
      d[d.min] <- 0
      d[d.max] <- 1
      # reassign as character string
      dt[i] <- as.character(d)
    }
  }
  return(dt)
}

################################################################################################################################


#####
# mainly internal function
conc.df <- function(x){
  D <- NULL
  taxa <- as.character(x[,1])
  dm <- x[-1]
  tp <- length(grep("[[:digit:]]",dm[1,]))>0
  n <- max(nchar(taxa))
  for(i in 1:nrow(x)){
    tx <- as.character(x[i,1])
    nsp <- n - nchar(tx) + 5
    ch <- as.character(x[i,-1])
    if(tp){
      ch <- gsub("NA", "?", ch)
    }
    ch <- paste(ch, collapse="")
    d <- paste(tx, paste(rep(" ", nsp), collapse=""), ch, sep="")
    D <- c(D, d)
  }
  return(D)
}

################################################################################################################################


##### function to convert a data.frame object to either a NEXUS or TNT file format
### expected input is phylogenetic character data in data.frame object
### for instance data.frame objects from dummycode or phylo2df
### or spreadsheet data read into R as data.frame object
### header (i.e., names(x)) is not necessary but shouldn't be touched if using dummycode output
df2phylo <- function(x, format=c("NEXUS", "TNT"), charlab=F){
  tx <- x[1]
  dm <- x[-1]
  type <- length(grep("[[:digit:]]",dm[1,]))>0
  ch <- NULL
  for(i in 1:nrow(dm)){
    d <- dm[i,]
    d <- paste(d, collapse="")
    ch <- c(ch, d)
  }
  ch <- gsub("[[:space:]]", "", ch)
  df <- data.frame(Taxon=tx, ch, stringsAsFactors=F)
  D <- conc.df(df)
  # count the number of character states
  if(type){
    dd <- alf <- NULL
    for(i in 1:ncol(dm)){
      di <- as.character(dm[,i])
      di <- unlist(strsplit(di,""))
      alf <- c(alf, di[grepl("[[:alpha:]]", di)])
      di <- gsub("[[:punct:]]|[[:alpha:]]", "", di)
      di <- as.numeric(di)
      dd <- c(dd, di)
    }
    dd <- dd[!is.na(dd)]
    num <- unique(dd)
    num <- num[order(num)]
    alf <- unique(alf)
    alf <- alf[order(alf)]
    num <- c(num, alf)
  }else{
    num <- NULL
  }
  if(format=="NEXUS"){
    D <- gsub("\\[|\\(", "\\{", D)
    D <- gsub("\\]|\\)", "\\}", D)
    # Format as nexus file
    h1 <- "#NEXUS"
    h2 <- "BEGIN DATA;"
    h3 <- paste("\tDIMENSIONS NTAX=", nrow(dm), " NCHAR=", ncol(dm), ";", sep="")
    if(type){
      h4 <- paste("\tFORMAT DATATYPE=STANDARD SYMBOLS= \"", paste(num, collapse=" "), "\" MISSING=? GAP=-;", sep="")
    }else{
      h4 <- paste("\tFORMAT DATATYPE=NUCLEOTIDE GAP=-;", sep="")
    }
    if(charlab){
      chnm <- paste("\tCHARLABELS ", paste(names(dm), collapse=" "), ";", sep="")
    }else{
      chnm <- NULL
    }
    h5 <- "\tMATRIX"
    t1 <- "\t;"
    t2 <- "END;"
    N <- c(h1, h2, h3, h4, chnm, h5, paste("\t",D,sep=""), t1, t2)
  }
  if(format=="TNT"){
    D <- gsub("\\(|\\{", "\\[", D)
    D <- gsub("\\)|\\}", "\\]", D)
    h1 <- "XREAD"
    h2 <- paste(ncol(dm), nrow(dm), sep=" ")
    t1 <- ";"
    N <- c(h1, h2, paste("\t",D,sep=""), t1)
  }
  return(N)
}

################################################################################################################################


