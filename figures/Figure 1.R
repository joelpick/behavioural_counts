rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

library(scales)
set.seed(3)

n <- 200
mean <- 20
sd <- 8
exp1 <- rep(mean,n)
obs1 <- rpois(n, exp1)
exp2 <- rlnorm(n, meanlog = log(mean), sdlog = sqrt(log(1+sd/mean^2)))
obs2 <- rpois(n, exp2)
Xmax <- max(c(obs1,obs2))

exp1Counts = hist(exp1, plot=FALSE, breaks=seq(-0.5,Xmax+0.5,1))$counts
obs1Counts = hist(obs1, plot=FALSE, breaks=seq(-0.5,Xmax+0.5,1))$counts
obs1Counts_tab = table(obs1)
exp2Counts = hist(exp2, plot=FALSE, breaks=seq(-0.5,Xmax+0.5,1))$counts
obs2Counts = hist(obs2, plot=FALSE, breaks=seq(-0.5,Xmax+0.5,1))$counts
obs2Counts_tab = table(obs2)

xSpace <- ySpace <- 5




setEPS()
pdf("figures/PR_fig1.pdf", height=10, width=10)

layout(matrix(c(1:12), ncol=3, byrow=FALSE),heights=c(1,2,4,2)/10, widths=c(2,4,4)/10)

par(mar=c(0,0,0,0), cex.lab=1.5)
plot(NA, yaxt="n", xaxt="n", bty="n", ylab="", xlab="")
plot(NA, yaxt="n", xaxt="n", bty="n", ylab="", xlab="")
text(0.5,0.05, "Expected", cex=3)
plot(NA, yaxt="n", xaxt="n", bty="n", ylab="", xlab="")
plot(NA, yaxt="n", xaxt="n", bty="n", ylab="", xlab="")
text(0.5,0.95, "Observed", cex=3)


plot(NA, yaxt="n", xaxt="n", bty="n", ylab="", xlab="")
text(0.5,0.5, "A) No variation in \n expected PR", cex=2.5)

par(mar=c(0,xSpace,0,1), las=2)
barplot(exp1Counts, xlim=c(0,Xmax)+0.5, ylim=c(0,n), space=0, ylab="Frequency")

par(mar=c(0,xSpace,0,1))
plot(rep(2,n)~exp1, xlim=c(0,Xmax), pch=19, cex=0.75, ylim=c(0.9,2.1), yaxt="n", xaxt="n", bty="n", ylab="")
#axis(2,c(1,2),c("Observed","Expected"), tick=FALSE, cex.axis=2)
points(rep(1,n)~obs1, pch=19, cex=0.5)
arrows(exp1,2,obs1,1, length=0.1, col=alpha("black",0.3))

par(mar=c(ySpace,xSpace,0,1))
# barplot(obs1Counts,  xlim=c(0,Xmax)+0.5, ylim=rev(c(0,n)), space=0,xlab="Provisioning Rate")
plot(obs1Counts_tab,  xlim=c(0,Xmax), ylim=rev(c(0,n/4)), bty="n", las=1, xaxt="n", ylab="Frequency",xlab="Provisioning Rate")
axis(1,0:Xmax, las=1)


par(mar=c(0,0,0,0))
plot(NA, yaxt="n", xaxt="n", bty="n", ylab="", xlab="")
text(0.5,0.5, "B) Variation in \n expected PR", cex=2.5)

par(mar=c(0,xSpace,0,1), las=2)
barplot(exp2Counts, xlim=c(0,Xmax)+0.5, ylim=c(0,n), space=0, ylab="Frequency")

par(mar=c(0,xSpace,0,1))
plot(rep(2,n)~exp2, xlim=c(0,Xmax), pch=19, cex=0.75, ylim=c(0.9,2.1), yaxt="n", xaxt="n", bty="n", ylab="")
#axis(2,c(1,2),c("Observed","Expected"), tick=FALSE, cex.axis=2)
points(rep(1,n)~obs2, pch=19, cex=0.5)
arrows(exp2,2,obs2,1, length=0.1, col=alpha("black",0.3))

par(mar=c(ySpace,xSpace,0,1))
# barplot(obs2Counts,  xlim=c(0,Xmax)+0.5, ylim=rev(c(0,n)), space=0,xlab="Provisioning Rate")
plot(obs2Counts_tab,  xlim=c(0,Xmax), ylim=rev(c(0,n/4)), bty="n", las=1, xaxt="n", ylab="Frequency",xlab="Provisioning Rate")
axis(1,0:Xmax, las=1)


dev.off()
