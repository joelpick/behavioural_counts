rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

load('lendvai_data/S2_File.RDATA')
head(visitdata)

#function to collect data.frames/matrices with different column names, and to specify those names
rbind_notAnnoying <- function(..., names=NULL){
	x <- list(...)
	y <- lapply(x, function(y){
    names(y) <- if(is.null(names)) names(x[[1]]) else names
    return(y)  
  } )
  do.call(rbind,y)
}

# function to determine if number is whole
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


#groups males and females by time period
visdat <- rbind_notAnnoying(visitdata[,1:60],visitdata[,62:121], names= paste(rep(7:21,each=4), rep(1:4,15), sep="_"))
head(visdat)


setEPS()
pdf("figures/PR_figS1.pdf", height=12, width=8)


par(mfrow=c(3,1), cex.axis=1.3, cex.lab=2, mar=c(6,6,4,1))
plot(apply(visdat,2,function(x) sum(is.na(x)))~ seq(7, 21.75, 0.25), xaxt="n", ylab="Number of NAs", xlab="Time of day (hour)", pch=19, col=c(rep(c(2,1),c(14,ncol(visdat)-14))), ylim=c(0,130))
abline(v=7+0.25*13.5, h=128, lty=2, col=c(1,2))
axis(1, 7:21)
mtext("A", 3, line=1, adj=0, cex=2)


plot(apply(visdat,2,function(x) sum(x==0,na.rm=TRUE))~ seq(7, 21.75, 0.25), xaxt="n", ylab="Number of zeros", xlab="Time of day (hour)", pch=19, col=c(rep(c(1,2),c(ncol(visdat)-5,5))), ylim=c(0,130))
abline(v=7+0.25*54.5, h=128, lty=2, col=c(1,2))
axis(1, 7:21)
mtext("B", 3, line=1, adj=0, cex=2)


x1 <- seq(10.5, 20.75, 0.25)

plot(NA, xlim=c(7,21.75), ylim=c(0,10),xlab="Time of day (hour)", bty="n", xaxt="n", yaxt="n", ylab="New observation period")

axis(2, 2:10, 2:10*15)
axis(1, 7:21)
mtext("C", 3, line=1, adj=0, cex=2)

arrows(x1[1:(length(x1)-1)],0.75,x1[2:length(x1)],0.75, length=0, col=c(7,2), lwd=3)

plot_arrows <- sapply(2:10, function(timeblocks){
	n_cols <- max((1:41)[is.wholenumber(1:41/timeblocks)])
	data <- x1
	data <- data[(length(data)-n_cols + 1):length(data)]	## preferentially miss out first coloumsn where there is missing data
	length(data)
	mat_timeblocks <- matrix(1:length(data),nrow=timeblocks)	
	arrows(rev(data[mat_timeblocks[1,]]-0.25), timeblocks, rev(data[mat_timeblocks[nrow(mat_timeblocks),]]), timeblocks, length=0.1, code=3, col=c(3,4), lwd=3)
} )



dev.off()