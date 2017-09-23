rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")



library(latex2exp) # enable to use LaTex in R expression
means <- 1:100				# range of mean visits over which which to plot


######------------------------------------------------------------

setEPS()
pdf("figures/PR_figI.pdf", height=5, width=5)

par(mar=c(6,6,1,1), cex.lab=1.5, cex.axis=1.2)

plot(sqrt(means)/means ~ means , type="l", ylab=TeX("$CV = \\frac{\\sigma_x}{\\bar{x}$"), xlab="Mean number of observed visits", ylim=c(0,1), lwd=2)
lines(rep(0.3, length(means)) ~ means, lty=2, lwd=2, col=1)

legend("topright", legend=c("Poisson", "Expected PR"), lty=c(1,2), col=c(1,1), cex=1.2)

dev.off()
