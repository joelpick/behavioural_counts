## install.packages("latex2exp")

rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

library(latex2exp) # enable to use LaTex in R expression
means <- 1:100				# range of mean visits over which which to plot
cvs <- seq(0.1,0.5,0.1)

CV_dat <- read.csv(file="lendvai_data/lendvai_CV.csv")
est_cv <- coef(nls(cv ~ sqrt((a*mean)^2 + mean)/mean, data=CV_dat, start=c(a=0.4)))
pois_cv <- sqrt((means*est_cv)^2 + means)/means

ICC_dat <- read.csv(file="lendvai_data/lendvai_ICC.csv")
gaus_rep <- coef(nls ( ICC ~ a*mean*est_cv^2 / (1+mean*est_cv^2), data=ICC_dat, start=list(a=0.5) ))


setEPS()
pdf("figure/PR_fig2.pdf", height=10, width=10)

par(mfrow=c(2,2), mar=c(3,6,3,1), cex.lab=1.75, cex.axis=1.2,oma = c(3, 0, 0, 0))


######------------------------------------------------------------


plot(NA, ylim=c(0,1), xlim=c(0,100), ylab="Observed CV", xlab="")
lapply(cvs, function(y) {
	lines( (sqrt((means*y)^2 + means)/means)~means, col=y*10+1, lwd=2) 
	lines(rep(y,each=length(means))~means, col=y*10+1, lty=2)
	})
legend("topright", c("Observed", "Expected"), lty=c(1,2), lwd=c(2,1), cex=1.2)
mtext('A',side=3, line=0.5, cex=1.5, adj=0)


######------------------------------------------------------------


plot(cv~mean, CV_dat, pch=19, ylim=c(0,1), xlab="", ylab="Observed CV")
lines(pois_cv~means, col="red")
abline(h=est_cv, lty=2)
text(50,0.95, paste("Estimated expected CV =", round(est_cv,3)), cex=1.5)
mtext('B',side=3, line=0.5, cex=1.5, adj=0)


######------------------------------------------------------------


plot(NA, ylim=c(0,1), ylab="Proportion of observed variance due\nto expected variation in provisioning rates", xlim=c(0,100), xlab="")

# plot line for each CV on expected scale 
lapply(cvs, function(x) {			
	exVar <- (means*x)^2 		# Intrinsic variance in provisioning rate
	obsVar <- means+exVar			# Observed variance in provisioning rate 
									# = intrinsic variance + poisson variance (mean)

	lines(exVar/obsVar~means, lty=x*10+1, lwd=2)		# add lines
})

legend("bottomright", legend=cvs, lty=cvs*10+1, title="Expected CV", cex=1.2)

mtext('C',side=3, line=0.5, cex=1.5, adj=0)


######------------------------------------------------------------


plot(ICC~mean,ICC_dat, ylim=c(0,1), pch=19, xlab="")
#points(ICC_pois~mean,ICC_dat, pch=19, col="red")
lines(gaus_rep * (means*est_cv)^2 / (means + (means*est_cv)^2 ) ~ means, col="red")
abline(h=gaus_rep, lty=2)
text(25,0.95, paste("Estimated ICC =", round(gaus_rep,3)), cex=1.5)
mtext('D',side=3, line=0.5, cex=1.5, adj=0)


mtext("Mean number of observed visits",line=0.5,side=1,outer=TRUE, cex=1.75)


dev.off()



#pois_rep <- mean(res$ICC_pois)
#lines((exVar/obsVar)*pois_rep~means, lty=2)	
#abline(h=pois_rep)

#abline(lm(ICC_pois~mean,res))
