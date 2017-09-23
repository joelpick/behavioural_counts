
mean=10;cv=0.3

est_cv <- function(mean,cv){
	N <- 100
	sd <- mean*cv
	meanLat <- log(mean)
	varLat <- log(1+sd^2/mean^2)
	lambda <- exp(rnorm(N,meanLat,sqrt(varLat)))
	visits <- rpois(N,lambda)

	est_cv <- sqrt(var(visits) - mean(visits))/mean(visits)
	c(est_cv,cv,mean)
}

nSims <- 10

sim_means <- (1:6)^2
sim_cvs <- seq(0.1,1,0.2)

out <- lapply(sim_means, function(mean) replicate(nSims,est_cv(mean,0.3)))

out2 <- array(unlist(out), dim=c(3,nSims,length(sim_means)))
 is.nan()
 apply(out2,c(1,3),mean)
 apply(out2,c(1,3),function(x)sum(is.nan(x))) 
 
 est_cv <- function(mean,cv){
	N <- 100
	sd <- mean*cv
	meanLat <- log(mean)
	varLat <- log(1+sd^2/mean^2)
	lambda <- exp(rnorm(N,meanLat,sqrt(varLat)))
	visits <- rpois(N,lambda)

	est_cv <- sqrt(var(visits) - mean(visits))/mean(visits)
	return(est_cv)
}

nSims <- 1000
sim_means <- seq(1,10,0.5)^2
sim_cvs <- seq(0.1,1,0.1)

out <- lapply(sim_cvs, function(cv) sapply(sim_means, function(mean) replicate(nSims,est_cv(mean,cv))))
lapply(out,head)

out2 <- array(unlist(out), dim=c(nSims,length(sim_means),length(sim_cvs)))
dim(out2)
 
 apply(out2,c(2,3),mean, na.rm=TRUE)

 apply(out2,c(2,3),function(x) sum(is.nan(x)))
 apply(out2,c(2),function(x) sum(is.nan(x)))/(10*1000)
 plot( apply(out2,c(2),function(x) sum(is.nan(x)))/(length(sim_cvs)*nSims)~sim_means, ylim=c(0,0.20), pch=19)

par(mfrow=c(1,2))

 
 image.plot(x=sim_means, y= sim_cvs, z=apply(out2,c(2,3),function(x)sum(is.nan(x)))/1000, main="NaNs", col=rev(heat.colors(20)))

 
par(mfrow=c(1,2),mar=c(5,5,5,1))
image.plot(x=sim_means, y= sim_cvs, z=apply(out2,c(2,3),function(x)sum(is.nan(x))), main="Bias")


bias <- abs(matrix(sim_cvs,ncol=length(sim_cvs),nrow=length(sim_means),byrow=TRUE) - apply(out2,c(2,3),mean, na.rm=TRUE))
precision <- 1/apply(out2,c(2,3),sd, na.rm=TRUE)
 library(fields)
# s <- interp(x=sim_means, y= sim_cvs, z=bias)

par(mfrow=c(1,2),mar=c(5,5,5,1))
image.plot(x=sim_means, y= sim_cvs, z=bias, main="Bias")

par(mar=c(5,1,5,5))
image.plot(x=sim_means, y= sim_cvs, z=precision, main="Precision", yaxt="n")