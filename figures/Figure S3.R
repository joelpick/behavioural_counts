rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

library(pbapply)
library(latex2exp) # enable to use LaTex in R expression
library(fields)

set.seed(25)
nSims <- 1000
sim_means <- seq(1,10,0.5)^2
sim_cvs <- seq(0.1,1,0.1)

load("simulations/expCV_bias_precision.Rdata")
out2 <- array(unlist(out_cv), dim=c(nSims,length(sim_means),length(sim_cvs)))
dim(out2)

load("lit_review/extracted_lit_review.Rdata")

apply(out2,c(2,3),function(x) sum(is.nan(x)))
apply(out2,c(2),function(x) sum(is.nan(x)))/(10*1000)
#image.plot(x=sim_means, y= sim_cvs, z=apply(out2,c(2,3),function(x)sum(is.nan(x)))/1000, main="NaNs", col=rev(heat.colors(20)))

mround <- function(x,base) base*round(x/base) 

setEPS()
pdf("figures/PR_figS3.pdf", height=5, width=10)

par(mfrow=c(1,2), mar=c(5,5,2,1), cex.lab=1.5, cex.axis=1.25)
plot( apply(out2,c(2),function(x) sum(is.nan(x)))/(length(sim_cvs)*nSims)~sim_means, ylim=c(0,0.20), pch=19, xlab="", ylab=TeX("Proportion where $\\sigma_x<\\bar{x}$"))
mtext("A",3, line=0, adj=0, cex=2)

plot(aggregate(is.nan(exp_cv)~mround(new_mean,1), dat2, sum), ylim=c(0,5), xlim=c(0,100), pch=19, xlab="", ylab=TeX("Number where $\\sigma_x<\\bar{x}$"))
mtext("B",3, line=0, adj=0, cex=2)
mtext("Mean number of Observed Visits",1, outer=TRUE, line=-2, cex=1.5)

dev.off()

