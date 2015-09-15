pseudoR2.MCMCglmm <- function(mod, data){
  # create model matrix using the MCMCglmm model and data objects
  dt <- model.matrix(summary(mod)$fixed.formula, data=data)
  # set logical condition to identify "Intercept" column in model matrix
  cond <- grep("Intercept", colnames(dt))
  # if "Intercept" exists, then subset the remaining columns without dropping dimensions for a one-column matrix
  # this will create a one-column matrix instead of a vector
  if(sum(cond)>0){dt <- dt[,-cond, drop=F]}
  
  # Identify variable names making sure they match the output
  cols <- colnames(mod$Sol)
  # set condition where colnames in mod$Sol match those in model matrix
  cond <- is.element(cols, colnames(dt))
  # subset mod$Sol to only columns matching those in model matrix
  sol <- mod$Sol[,cond, drop=F]
  # do the oposite: subset model matrix where colnames match colnames in mod$Sol
  cond <- is.element(colnames(dt), cols)
  dt <- dt[,cond, drop=F]
  
  # First calculate the variance associated with the fixed effects
  betas <- colMeans(sol)
  xs <- t(dt) * betas
  bx <- colSums(xs)
  sigf <- var(bx)
  
  # Then, calculate the variance associated with the random effects
  mod.vcv <- data.frame(mod$VCV[, 1:(ncol(mod$VCV)-1)])
  sigr <- sum(as.numeric(colMeans(mod.vcv)))
  
  # Now, calculate the variance associated with the residual effects
  sige <- mean(mod$VCV[,"units"])
  
  # marginal R2 (variance explained by fixed factors)
  r2m.i <- sigf / {sigf + sige + sigr}
  # conditional R2 (variance explained by the entire model)
  r2c.i <- {sigf + sigr} / {sigf + sige + sigr}
  r2 <- c(r.squared.marg=r2m.i, r.squared.cond=r2c.i)
  return(r2)
}