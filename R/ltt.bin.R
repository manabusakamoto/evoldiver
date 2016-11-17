# function to create lineage through time bins
ltt.bin <- function(phy, t, ...){
  require(ape)
  require(data.table)
  ages <- nodeAges(phy, ...)
  
  anc <- phy$edge[, 1]
  dec <- phy$edge[, 2]
  
  anc <- data.table(node=as.character(anc))
  dec <- data.table(node=as.character(dec))
  
  age.node <- data.table(node=names(ages$nodeage), age=ages$nodeage)
  age.tip <- data.table(node=names(ages$tip.age), age=ages$tip.age)
  age.dt <- list(age.tip, age.node)
  age.dt <- rbindlist(age.dt)
  
  anc.dt <- merge(anc, age.dt, by="node")
  setnames(anc.dt, paste("Anc", names(anc.dt), sep="_"))
  dec.dt <- merge(dec, age.dt, by="node")
  setnames(dec.dt, paste("Dec", names(dec.dt), sep="_"))
  
  anc.dt <- anc.dt[match(as.character(phy$edge[, 1]), Anc_node), ]
  dec.dt <- dec.dt[match(as.character(phy$edge[, 2]), Dec_node), ]
  
  ages.dt <- data.table(anc.dt, dec.dt)
  
  a <- ages.dt$Anc_age
  d <- ages.dt$Dec_age
  n <- numeric(length(t)-1)
  for(i in 2:length(t)){
    .t1 <- t[i - 1]
    .t2 <- t[i]
    
    cond1 <- a >= .t2 & {d <= .t2 & d >= .t1}
    cond2 <- {a <= .t2 & a > .t1} & {d < .t2 & d >= .t1}
    cond3 <- {a <= .t2 & a >= .t1} & d <= .t1
    cond4 <- a >= .t2 & d <= .t1
    
    cond <- cond1 | cond2 | cond3 | cond4
    
    n[i-1] <- nrow(ages.dt[cond, ])
  }
  lttb <- rev(n)
  names(lttb) <- paste("t", 1:length(t[-1]), sep = "_")
  return(lttb)
}
