rm(list=ls())

options(width=Sys.getenv("COLUMNS"))

arrivals <- function(obsTime=90, beta=0.281, alpha=1.08){
	visits <- rgamma(1000, rate=beta, shape=alpha)
	csVisits <- cumsum(visits)
	return(csVisits[csVisits<obsTime])
	}


setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials/figures")

setEPS()
pdf("PR_fig1.pdf", height=5, width=12)

set.seed(56)
obsTime <- 180

# layout(matrix(1:2, nrow=2),height=c(1,3))
# par(mar=c(2,6,2,2), cex.lab=2, cex.axis=1.5)
# plot(NA, ylim=c(0.5,1.5), xlim=c(0,obsTime), ylab="Nest observation", xlab="Time (mins)", bty="n", yaxt="n", xaxt="n")


par(mar=c(6,6,2,2), cex.lab=2, cex.axis=1.5)
plot(NA, ylim=c(0.5,7.5), xlim=c(0,obsTime), ylab="Nest observation", xlab="Time (mins)", bty="n", yaxt="n", xaxt="n")
axis(2,1:6, 6:1, tick=FALSE, las=2)
axis(1,seq(0,obsTime,length.out=7), seq(0,obsTime,length.out=7))

abline(h=1:6,col="grey")

for(i in seq(0,obsTime,length.out=7)) lines(x=rep(i,2),y=c(0.25,6.5), lty=2, col="red")

aa <- lapply(1:6, function(i){
	x <- arrivals(obsTime, 0.075, 1)
	points(rep(i, length(x))~x, pch=19)
	return(x)
})
arrows(aa[[6]][5],6.5,aa[[6]][6],6.5, code=3, length=0.05)
text(mean(aa[[6]][5:6]),7,"Interval", cex=1.5)

legend("topleft", legend="Arrival", pch=19, bty="n", cex=1.5)


dev.off()




setEPS()
pdf("PR_fig1a.pdf", height=3.5, width=8)

set.seed(556)
obsTime <- 180


par(mar=c(6,2,2,2), cex.lab=1.75, cex.axis=1.5)
plot(NA, ylim=c(0.5,2), xlim=c(0,obsTime), ylab="", xlab="Time (mins)", bty="n", yaxt="n", xaxt="n")
# axis(2,1:6, 6:1, tick=FALSE, las=2)
axis(1,seq(0,obsTime,length.out=7), seq(0,obsTime,length.out=7))

abline(h=1,col="grey")

for(i in seq(0,obsTime,length.out=7)) lines(x=rep(i,2),y=c(0.25,1.4), lty=2, col="red")
x <- arrivals(obsTime, 0.075, 1)
points(rep(1, length(x))~x, pch=19)

arrows(x[7],1.5,x[8],1.5, code=3, length=0.05)
text(mean(x[7:8]),1.7,"Interval", cex=1.5)

legend("topleft", legend="Arrival", pch=19, bty="n", cex=1.5)


dev.off()


