rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

library(latex2exp) # enable to use LaTex in R expression

library(tweedie)
#install.packages("tweedie")

visits <- function(obsTime=90, beta=0.281, alpha=1.08){
	visits <- rgamma(1000, rate=beta, shape=alpha)
	csVisits <- cumsum(visits)
	nVisits <- length(csVisits[csVisits<obsTime])
	return(nVisits)
	}


obsTime <- 90
mean <- 5
a <- c(1,3,5,7)
b <- a/mean
meanN <- 90/mean


set.seed(5)
sameMean <- lapply(1:length(a), function(x) replicate(1000,visits(obsTime=obsTime, beta=b[x],alpha=a[x])))



setEPS()
pdf("figures/PR_figS2.pdf", height=12, width=12)

par(mfcol=c(length(a),4), cex.lab=1.5) 


x <- lapply(1:length(a), function(x){
	plot(seq(0,20,0.1),dgamma(seq(0,20,0.1),  rate=b[x], shape=a[x]), type="l", ylab="Probability density", xlab="Interval length", ylim=c(0,0.25))
	if(x==1) mtext("A", side=3,adj=0, line=0.5)
	})
 
x <- lapply(1:length(a), function(x) {
	plot(table(sameMean[[x]]), xlim=c(0,40), ylim=c(0,250), main=paste("Alpha =", a[x]), xlab="Number of events", col="darkgrey", cex.main=2, xaxt="n")
	axis(1,seq(0,40,5))
	abline(v=mean(sameMean[[x]]), lwd=3, lty=2, col=2)
	if(x==1) mtext("B", side=3,adj=0, line=0.5)
	})

x <- lapply(1:length(a), function(x) {
	plot(seq(0,40,1), dtweedie(y=seq(0,40,1), mu=18, phi=1/x, power=1), type="l", ylab="Probability density", xlab="Number of events", ylim=c(0,0.2))
	if(x==1) mtext("C", side=3,adj=0, line=0.5)
	})


plot(a,sapply(sameMean, var), pch=19, cex=2, ylim=c(0,20), ylab="Sampling Variance", xlab="Alpha"); lines(seq(min(a),max(a),0.1),obsTime/(mean*seq(min(a),max(a),0.1))); text(a[3],16, TeX("$\\sigma^2_e = \\frac{t}{\\alpha\\mu}$"), cex=2)
mtext("D", side=3,adj=0, line=0.5)



dev.off()