predNew.MCMCglmm <- function(MCMCglmm, newdata){
  formula <- MCMCglmm$Fixed$formula[-2]
  vnms <- all.vars(formula)
  cond <- is.element(names(newdata), vnms)
  nd.mod <- newdata[,cond,with=F]
  mod.mat <- model.matrix(formula, data=nd.mod)
  
  # Identify variable names making sure they match the output
  cols <- colnames(MCMCglmm$Sol)
  cond <- is.element(colnames(mod.mat), cols)
  mod.mat <- mod.mat[,cond]
  
  sol <- MCMCglmm$Sol
  beta.mean <- colMeans(sol)
  pred.mean <- colSums(t(mod.mat) * beta.mean)
  pred.mean <- exp(t(pred.mean))
  pred.dt <- data.table(newdata, predicted_mean=pred.mean[1,])
  
  pred.mat <- matrix(ncol=nrow(sol), nrow=nrow(newdata))
  for(j in 1:nrow(sol)){
    betas <- sol[j,]
    pred <- colSums(t(mod.mat) * betas)
    pred <- exp(t(pred))
    pred.mat[,j] <- t(pred)
  }# end loop j
  colnames(pred.mat) <- paste("predicted", seq(1:nrow(sol)), sep="_")
  
  return(pred.ls <- list(data=newdata, mean_predictions=pred.mean[1,], MCMC_predictions=data.table(pred.mat)))
}
