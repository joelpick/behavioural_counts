rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

load("lit_review/extracted_lit_review.Rdata")

library(scales)

obsN_rep <- rep(dat2$new_mean, dat2$N_analysis)
exp_cv_rep <- rep(dat2$exp_cv, dat2$N_analysis)
obs_prop <- rep(dat2$prop_w0, dat2$N_analysis)

propExpVar <- function(mean,cv){
	exVar <- (mean*cv)^2
	obsVar <- mean+exVar
	return(exVar/obsVar)
}


sym_dat <- na.omit(dat2[,c("new_mean","prop_w0","N_analysis","direct")])

sym_legend <- function(x,y, legend, yspace=0.07, area=legend, inches=0.25, bg=alpha(1,0.5),title="No. Analyses"){
	#y_coords <- seq(from=y, by=-1*yspace, length.out=length(legend))
	y_coords <- y - (2:(length(legend)+1))^2*yspace
	symbols(rep(x,length(legend)),y_coords, circles=sqrt(area/pi), inches=inches, bg=bg,add=TRUE)
	text(rep(x+10,length(legend)),y_coords, legend)
	text(x+5,y+yspace, title, cex=1.25)
} 


setEPS()
pdf("figures/PR_fig4.pdf", height=10, width=10)

###################------------------------------------------------------------
{
par(mar=c(0,6,1,0), cex.lab=1.5, oma=c(0,0,0,0))
layout(mat=matrix(1:4, ncol=2), heights=c(3,10), widths=c(10,3))

#Ncounts <- hist(obsN, breaks=20, col="grey", main="", ylab="Number of Estimates", xlab="Mean number of observed visits", xlim=c(0,140), xaxt="n")$counts;abline(v=median(obsN, na.rm=TRUE), col="red")
#axis(4,seq(5,max(Ncounts),5))#, yaxt="n"

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

obs2Counts <- hist(obsN_rep, plot=FALSE, breaks=seq(0,ifelse(is.wholenumber(max(obsN_rep,na.rm=TRUE)/10), max(obsN_rep,na.rm=TRUE), max(obsN_rep,na.rm=TRUE)+10),10))$counts
barplot(obs2Counts[1:17], space=0, ylab="Number of analyses", xlab="")

abline(v=median(obsN_rep, na.rm=TRUE)/10, col="blue")


###################------------------------------------------------------------

par(mar=c(6,6,0,0))

symbols(sym_dat$new_mean,sym_dat$prop_w0, circle= sqrt( (sym_dat$N_analysis)/pi ), inches=0.25, bg=alpha(c(2,1),0.5)[as.factor(sym_dat$direct)], ylim=c(0,1), xlim=c(0,170), ylab="Proportion of observed variance due\nto expected variation in provisioning rates", xlab="Mean number of observed visits")

means <- 1:170				# range of mean visits over which which to plot
cvs <- seq(0.1,1,0.2)		# coefficents of variation of expected provisioning rates 

# plot line for each CV on expected scale 
em <- lapply(cvs, function(x) {			
	y <- propExpVar(means,x)
	lines(y~means, lty=x*10+1, lwd=0.5)		# add lines
})

legend("bottomright", legend=cvs, lty=cvs*10+1, title="Expected CV", cex=1.2, bty="n")
sym_legend(158, 0.52, c(1,2,4,8,16), yspace=0.0075, inches=0.2)

abline(v=median(obsN_rep, na.rm=TRUE), col="blue")
abline(h=median(obs_prop,na.rm=TRUE),col="blue")

###################------------------------------------------------------------

par(mar=c(0,0,0,0))
plot(NA, xlim=c(-1,1), ylim=c(-1,1), xaxt="n", yaxt="n", bty="n"); # text(0,0,"Number of Estimates")

###################------------------------------------------------------------

par(mar=c(6,0,0,1))
prop2Counts <- hist(obs_prop, breaks=20, plot=FALSE)$counts
barplot(prop2Counts, horiz=TRUE, space=0, ylab="", xlab="Number of analyses")
abline(h=median(obs_prop*20,na.rm=TRUE),col="blue")
#axis(3,seq(min(prop2Counts),max(prop2Counts),1))
}



dev.off()






