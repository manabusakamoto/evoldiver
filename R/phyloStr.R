#####	Function phyloStr
#####	Univariate variance partitioning	#####
phyloStr <- function(y, x, phyloeig){
	x <- as.matrix(x)
	phyloeig <- as.matrix(phyloeig)
	r <- y
#	comb <- data.frame(x,phyloeig)
	regress1 <- lm(r ~ x + phyloeig)
	regress2 <- lm(r ~ phyloeig)
	regress3 <- lm(r ~ x)
#	regress1 <- lm(r ~ ., data=comb)
#	regress2 <- lm(r ~ ., data=phyloeig)
#	regress3 <- lm(r ~ ., data=x)
	s1 <- summary(regress1)
	s2 <- summary(regress2)
	s3 <- summary(regress3)
	R1 <- s1$r.squared
	R2 <- s2$r.squared
	R3 <- s3$r.squared
	p1 <- pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail = FALSE)
	p2 <- pf(s2$fstatistic[1], s2$fstatistic[2], s2$fstatistic[3], lower.tail = FALSE)
	p3 <- pf(s3$fstatistic[1], s3$fstatistic[2], s3$fstatistic[3], lower.tail = FALSE)
	a <- R1 - R2
	b <- R3 - a
	c <- R2 - b
	d <- 1 - R1
	p.values <- c(p3,p2,p1)
	names(p.values) <- c("y~x","y~phylo","y~x+phylo")
	r2 <- c(R3,R2,R1)
	names(r2) <- c("y~x","y~phylo","y~x+phylo")
	aic.lm <- c(AIC(regress3),AIC(regress2),AIC(regress1))
	names(aic.lm) <- c("y~x","y~phylo","y~x+phylo")
	wi <- AkaikeWeights(aic.lm)
	results1 <- rbind(r2,p.values,wi)
	results2 <- c(a,b,c,d)
	names(results2) <- c("a","b","c","d")
	results <- list(lm.x=s3, lm.phylo=s2, lm.x.phylo=s1, stats=results1, var.partitions=results2)
	return(results)
}