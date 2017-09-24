rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")


library(pbapply)


######
##----Functions
######

sim_data <- function(mean,cv){
	N <- 100
	R2_PR <- 0.5
	sd <- mean*cv
	meanLat <- log(mean)
	varLat <- log(1+sd^2/mean^2)
	rain <- rnorm(N, 0, sqrt(R2_PR*varLat))
	lambda <- exp(rnorm(N,meanLat + rain,sqrt(varLat - R2_PR*varLat)))
	visits <- rpois(N,lambda)
	data <- data.frame(visits,rain)
}

est_cv <- function(visits)	sqrt(var(visits) - mean(visits))/mean(visits)

R2_pois_est <- function(data){
	require(lme4)
	data$obs <- 1:nrow(data)
	mod2 <- glmer(visits~rain + (1|obs),data,family="poisson")
	R2_pois <- var(model.matrix(mod2) %*% summary(mod2)$coef[,1])/(var(model.matrix(mod2) %*% summary(mod2)$coef[,1]) + as.numeric(summary(mod2)$var$obs))
	return( R2_pois)
}


######
##----Datasets
######

set.seed(25)
nSims <- 1000
sim_means <- seq(1,10,0.5)^2
sim_cvs <- seq(0.1,1,0.1)

datasets <- pblapply(sim_cvs, function(cv) lapply(sim_means, function(mean) replicate(nSims,sim_data(mean,cv), simplify=FALSE)))

 out_cv <- pblapply(datasets, function(z) sapply(z, function(y) sapply(y, function(x) head)))


######
##----Expected CV
######

out_cv <- pblapply(datasets, function(cv) sapply(cv, function(mean) sapply(mean, function(x) est_cv(x$visits))))
save(out_cv, file="simulations/expCV_bias_precision.Rdata")


######
##----R2
######

out_R2 <- pblapply(datasets, function(cv) sapply(cv, function(mean) sapply(mean, R2_pois_est)))
save(out_R2, file="simulations/R2_pois_est.Rdata")
