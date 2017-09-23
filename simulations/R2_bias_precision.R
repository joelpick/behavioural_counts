R2_pois_est <- function(mean,cv){
	require(lme4)
	N <- 100
	sd <- mean*cv
	R2_PR <- 0.5
	meanLat <- log(mean)
	varLat <- log(1+sd^2/mean^2)
	rain <- rnorm(N, 0, sqrt(R2_PR*varLat))
	lambda <- exp(rnorm(N,meanLat + rain,sqrt(varLat - R2_PR*varLat)))
	visits <- rpois(N,lambda)
	data <- data.frame(visits,rain)
	data$obs <- 1:nrow(data)
	mod2 <- glmer(visits~rain + (1|obs),data,family="poisson")
	R2_pois <- var(model.matrix(mod2) %*% summary(mod2)$coef[,1])/(var(model.matrix(mod2) %*% summary(mod2)$coef[,1]) + as.numeric(summary(mod2)$var$obs))
	return( R2_pois)
}

library(pbapply)

set.seed(25)

nSims <- 1000
sim_means <- seq(1,10,0.5)^2
sim_cvs <- seq(0.1,1,0.1)

out_R2 <- pblapply(sim_cvs, function(cv) sapply(sim_means, function(mean) replicate(nSims,R2_pois_est(mean,cv))))
lapply(out_R2,head)
# save(out_R2, file="~/Dropbox/0_postdoc/8_PR repeat/shared/figures/R2_pois_est.Rdata")

out_R2_2 <- array(unlist(out_R2), dim=c(nSims,length(sim_means),length(sim_cvs)))
dim(out_R2_2)


bias_R2 <- 0.5 - apply(out_R2_2,c(2,3),mean, na.rm=TRUE)
precision_R2 <- 1/apply(out_R2_2,c(2,3),sd, na.rm=TRUE)
library(fields)
# s <- interp(x=sim_means, y= sim_cvs, z=bias)

par(mfrow=c(1,2),mar=c(5,5,5,5), cex.lab=1.5, cex.axis=1.2)
image.plot(x=sim_means, y= sim_cvs, z=bias_R2, ylab="Expected CV", xlab="Mean number of observed visits", main="Bias", legend.mar=10)

par(mar=c(5,5,5,5))
image.plot(x=sim_means, y= sim_cvs, z=precision_R2, main="Precision", yaxt="n", ylab="", legend.lab="precesion")