#####	Function mv.phyloStr
#####	multivariate variance partitioning	#####
mv.phyloStr <- function(y,x,phyloeig){
	r <- as.matrix(y)
	phylo <- as.matrix(phyloeig)
	pheno <- as.matrix(x)
	predictors <- cbind(pheno,phylo)
	manova.ab <- manova(r ~ pheno)
	manova.bc <- manova(r ~ phylo)
	manova.abc <- manova(r ~ predictors)
	wilks.ab <- summary(manova.ab, test=c("Wilks"))
	wilks.bc <- summary(manova.bc, test=c("Wilks"))
	wilks.abc <- summary(manova.abc, test=c("Wilks"))
	p.ab <- wilks.ab$stats[1,6]
	p.bc <- wilks.bc$stats[1,6]
	p.abc <- wilks.abc$stats[1,6]
	p.values <- c(p.ab,p.bc,p.abc)
	names(p.values) <- c("y~x","y~phylo","y~x+phylo")
	mlm.ab <- lm(r ~ pheno)
	mlm.bc <- lm(r ~ phylo)
	mlm.abc <- lm(r ~ pheno+phylo)
	aic.mlm <- c(AIC(mlm.ab),AIC(mlm.bc),AIC(mlm.abc))
	names(aic.mlm) <- c("y~x","y~phylo","y~x+phylo")
	wi.mlm <- AkaikeWeights(aic.mlm)
	var.r <- sum(apply(r,2,sd)^2)
	var.ab <- sum(apply(mlm.ab$fitted.values,2,sd)^2)
	var.bc <- sum(apply(mlm.bc$fitted.values,2,sd)^2)
	var.abc <- sum(apply(mlm.abc$fitted.values,2,sd)^2)
	prop.var.ab <- var.ab/var.r
	prop.var.bc <- var.bc/var.r
	prop.var.abc <- var.abc/var.r
	part.a <- prop.var.abc - prop.var.bc
	part.b <- prop.var.ab - part.a
	part.c <- prop.var.bc - part.b
	part.d <- 1 - prop.var.abc
	partitions <- c(part.a,part.b,part.c,part.d)
	names(partitions) <- c("a","b","c","d")
	r2 <- c(prop.var.ab,prop.var.bc,prop.var.abc)
	names(r2) <- c("y~x","y~phylo","y~x+phylo")
	stats <- rbind(r2,p.values,wi.mlm)
	results <- list(mlm.x=summary(mlm.ab), mlm.phylo=summary(mlm.bc), mlm.x.phylo=summary(mlm.abc), stats=stats, var.partitions=partitions)
	return(results)
}