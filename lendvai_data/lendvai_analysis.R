########
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


par(mfrow=c(2,2))
plot(apply(visdat,2,function(x) sum(is.na(x)))~ seq(7, 21.75, 0.25), ylab="number fo NAs", xlab="Time of day (hour)"); abline(v=7+0.25*13.5)
plot(apply(visdat,2,function(x) sum(x==0,na.rm=TRUE))~ seq(7, 21.75, 0.25), ylab="number fo zeros", xlab="Time of day (hour)"); abline(v=7+0.25*54.5)
# take off last 5 15 min slots, as they have high number of 0s - i.e. everything has stopped provisioning!! 
# take off first 15 as lots of NAs - want to maximise sample size

visdat2 <- as.matrix(visdat[,15:55])
colnames(visdat2)
dim(visdat2)



## create new data sets, made from suming all visits within increasing long observation periods
timeblock_data <- lapply(1:ncol(visdat2), function(timeblocks){
	data <- visdat2
	n_cols <- max((1:ncol(data))[is.wholenumber(1:ncol(data)/timeblocks)]) ## work out number of columns to use; what is the maximum number that is divisible by that timeblock
	data <- data[,(ncol(data)-n_cols + 1):ncol(data)]	## preferentially miss out first coloumsn where there is missing data
	mat_timeblocks <- matrix(1:ncol(data),nrow=timeblocks)	## which columns to add together in the new time blocks
	dat_timeblocks <- sapply(1:ncol(mat_timeblocks),function(x) apply(as.matrix(data[,mat_timeblocks[,x]]),1,sum) )	## adds together number of visits in each new time block, each column is now a new timeblock
	dat_out <- do.call(rbind, lapply(1:ncol(dat_timeblocks), function(x) data.frame(id=1:nrow(dat_timeblocks), time_period=x, visits=dat_timeblocks[,x]))) ## stack new timeblocks, to give 'long' dataframe for analysis
	return(dat_out)
})



CV_dat <- as.data.frame(t(sapply(1:41, function(x) {
	data <- timeblock_data[[x]]
	c(time=x*15,mean=mean(data$visits, na.rm=TRUE),sd=sd(data$visits, na.rm=TRUE), n=length(!is.na(data$visits)))
})))
CV_dat$cv <- with(CV_dat, sd/mean)
CV_dat


## data sets that have multiple time periods in them, for repeatability
data_for_ICC <- which(sapply(timeblock_data, function(x) max(x$time_period))>1)


ICC_dat <- as.data.frame(t(sapply(data_for_ICC, function(x) {
	data <- timeblock_data[[x]]
	mod <- lmer(visits~ 1+ (1|id), data)
	id_var <- as.numeric(summary(mod)$var$id)
	resid_var <- as.numeric(summary(mod)$sigma)^2	
	ICC <- id_var/(id_var+resid_var)
	return(c(time=x*15, ICC=ICC, mean=mean(data$visits,na.rm=TRUE)))
})))



## write.csv(CV_dat, file="lendvai_data/lendvai_CV.csv", row.names=FALSE)
## write.csv(ICC_dat, file="lendvai_data/lendvai_ICC.csv", row.names=FALSE)
