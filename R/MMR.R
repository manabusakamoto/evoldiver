#####	multivariate linear model
MMR <- function(y, x, test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")){
	Y <- as.matrix(y)
	X <- as.matrix(x)
	sMANOVA <- summary(manova(Y ~ X),test=test)
	mod <- lm(Y ~ X)
	VY <- sum(apply(Y,2,sd)^2)
	Vmod <- sum(apply(mod$fitted.values,2,sd)^2)
	pV <- Vmod/VY
	results <- c(pV, sMANOVA$stats[1,6])
	names(results) <- c("R2","p-value")
	return(results)
}