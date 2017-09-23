rm(list=ls())

library(fields)
library(latex2exp) # enable to use LaTex in R expression

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

R2_response <- read.csv(file="simulations/R2_response.csv")
R2_predictor <- read.csv(file="simulations/R2_predictor.csv")

load(file="simulations/R2_pois_est.Rdata")

setEPS()
pdf("figures/PR_fig5.pdf", height=10, width=12)

layout(matrix(c(1,1,2,2,3:6), nrow=2, byrow=TRUE), width=c(1,5,5,1))

par(mar=c(2,5,3,1), cex.lab=1.75, cex.axis=1.4,oma = c(4, 0, 0, 0))

mean_obs <- 1:100
plot(R2_lm~mean,R2_response,ylim=c(0,1),col="black",pch=19, xlab="", ylab=TeX("$R^2$"))
points(R2_pois~mean,R2_response,ylim=c(0,1),col="red",pch=19)
abline(h=0.5)
lines(0.5*(mean_obs*0.3^2)/(mean_obs*0.3^2+1)~mean_obs, lty=2)
mtext('A',side=3, line=0.5, cex=1.5, adj=0)

plot(R2_lm~mean,R2_predictor,ylim=c(0,1),col="black",pch=19, xlab="", ylab=TeX("$R^2$"))
points(R2_mem~mean,R2_predictor,ylim=c(0,1),col="red",pch=19)
abline(h=0.5)
lines(0.5*(mean_obs*0.3^2)/(mean_obs*0.3^2+1)~mean_obs, lty=2)
mtext('B',side=3, line=0.5, cex=1.5, adj=0)



nSims <- 1000
sim_means <- seq(1,10,0.5)^2
sim_cvs <- seq(0.1,1,0.1)

out_R2_2 <- array(unlist(out_R2), dim=c(nSims,length(sim_means),length(sim_cvs)))
bias_R2 <- apply(out_R2_2,c(2,3),mean, na.rm=TRUE) - 0.5
precision_R2 <- 1/apply(out_R2_2,c(2,3),sd, na.rm=TRUE)


par(mar=c(3,6,4,1))
image(x=1, y= seq(min(bias_R2),max(bias_R2),0.01), z=matrix(seq(min(bias_R2),max(bias_R2),0.01),nrow=1), ylab=TeX("$E(\\hat{\\theta}) - \\theta$"), xlab="", xaxt="n", col=tim.colors())
mtext('C',side=3, line=0.5, cex=1.5, adj=0)

par(mar=c(3,1,4,5))
image(x=sim_means, y= sim_cvs, z=bias_R2, ylab="", xlab="", yaxt="n", col=tim.colors())
axis(4)

par(mar=c(3,5,4,1))
image(x=sim_means, y= sim_cvs, z=precision_R2, ylab="", xlab="", col=tim.colors())
mtext("Expected CV", side=2, line=4.5, cex=1.5)
mtext('D',side=3, line=0.5, cex=1.5, adj=0)

par(mar=c(3,1,4,6))
image(x=1, y= seq(min(precision_R2),max(precision_R2),0.01), z=matrix(seq(min(precision_R2),max(precision_R2),0.01),nrow=1), xaxt="n", yaxt="n", ylab=" ", xlab="", col=tim.colors())
axis(4)
mtext(TeX("$^1/_{\\sigma_{\\hat{\\theta}}}$"), side=4, line=4, cex=1.5)

mtext('Mean number of observed visits',side=1,outer=TRUE, line=2, cex=1.5)


dev.off()
 