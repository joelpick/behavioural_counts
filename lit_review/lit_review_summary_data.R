rm(list=ls())


####
#--- Functions
####

pasteV <- function(x,sep=" ") do.call(paste,c(as.list(x),sep=sep))

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

remove_trailing_s <- function(x) ifelse(substring(x,nchar(x)) == "s", substr(x,1,nchar(x)-1), x)


####
#--- Import Data
####

# library(googlesheets)
# dat <- gs_title("Lit_review_analyses")
# dat2 <- as.data.frame(gs_read(dat, ws = 1))
# write.csv(dat2,file="~/Dropbox/0_postdoc/8_PR repeat/shared/figures/lit_review_data_new.csv",row.names=FALSE)

dat2 <- read.csv(file="~/Dropbox/0_postdoc/8_PR repeat/shared/figures/lit_review_data_new.csv",stringsAsFactors=FALSE)
names(dat2)
nrow(dat2)




####
#--- Study Level Metrics
####

## obs method as binary
dat2$direct <- dat2$obs_method %in% c("direct","video","mixed direct + video")

## clean data availability
dat2$data_available2 <- grepl("TRUE",dat2$data_available)


summary_data <- list(
	studies = length(unique(dat2$ref)),
	obs_method = table(aggregate(species~obs_method+ref,dat2,length)$obs_method), 
	obs_method_bin = table(aggregate(species~direct+ref,dat2,length)$direct),
	single_obs_period = table(aggregate(species~single_obs_period_length+ref,dat2,length)$single_obs_period_length),
	data_available = table(aggregate(species~data_available2+ref,dat2,length)$data_available2),
	datasets = nrow(dat2)
)


####
#--- Species Names
####

## split up species in studies with two species
dat2$species1 <- sapply(strsplit(dat2$species,"/ "), function(x) x[1])
dat2$species2 <- sapply(strsplit(dat2$species,"/ "), function(x) x[2])

## extract species name
dat2$scientific_name1 <- gsub("\\(","",gsub("\\)","",sapply(strsplit(dat2$species1,"\ "), function(x) pasteV(x[(length(x)-1):length(x)]))))

dat2$scientific_name2 <- gsub("\\(","",gsub("\\)","",sapply(strsplit(dat2$species2,"\ "), function(x) pasteV(x[(length(x)-1):length(x)]))))

dat2$scientific_name <- ifelse(dat2$scientific_name2=="NA", dat2$scientific_name1, paste(dat2$scientific_name1,dat2$scientific_name2, sep=" / "))


## extract common name
## seperate out common name, then remove any commas, then remoe any trailing 's' and then capitalise words

dat2$common_name1 <- sapply(remove_trailing_s(gsub(",","",sapply(strsplit(dat2$species1,"\ "), function(x) pasteV(x[1:(length(x)-2)])))),simpleCap)

dat2$common_name2 <- gsub(",","", sapply(strsplit(dat2$species2, "\ "), function(x) ifelse(is.na(x[1]), NA, simpleCap(remove_trailing_s(pasteV(x[1:(length(x)-2)]))))))

dat2$common_name <- ifelse(is.na(dat2$common_name2), dat2$common_name1, paste(dat2$common_name1,dat2$common_name2, sep=" / "))

summary_data$species <- length(unique(c(dat2$scientific_name1,dat2$scientific_name2)))
length(unique(c(dat2$common_name1,dat2$common_name2))) == length(unique(c(dat2$scientific_name1,dat2$scientific_name2)))



#### 
#--- Methods
####

## analyses
dat2$N_Response1 <- as.numeric(sapply(strsplit(dat2$N_Response,"/"), function(x) if(is.na(x[1])) 0 else x[1]))
dat2$N_Response2 <- as.numeric(sapply(strsplit(dat2$N_Response,"/"), function(x) if(is.na(x[2])) 0 else x[2]))
dat2$N_Response_total <- dat2$N_Response1 + dat2$N_Response2
dat2$N_analysis <- apply(dat2[,c("N_Predictor","N_Response_total")],1,sum, na.rm=TRUE)

## analysis method
## LM, LMM, GLM, GLMM, non-parametric
## LM inclused regression, t-test, anova, correlation
dat2$method1 <- sapply(strsplit(dat2$methods,"/"), function(x) x[1])
dat2$method2 <- sapply(strsplit(dat2$methods,"/"), function(x) x[2])

## error dstribution used
## Gaussian, Poisson, Negative Binomial, Quasi-poisson, zero-inflated Poisson, gamma, non-parametric
dat2$error_dist1 <- sapply(strsplit(dat2$error_distribution,"/"), function(x) x[1])
dat2$error_dist2 <- sapply(strsplit(dat2$error_distribution,"/"), function(x) x[2])

## variable analysed
dat2$variable2 <- sapply(strsplit(dat2$variable,"/"), function(x){
	x[grepl("hour",x)|grepl("min",x)|grepl("day",x)] <- "time"
	pasteV(x, sep="/")
	})


summary_data$analyses <- apply(dat2[,c("N_Response_total","N_Predictor")],2,sum, na.rm=TRUE)
summary_data$methods <- table(c(rep(dat2$method1,dat2$N_Response1),rep(dat2$method2,dat2$N_Response2)))
summary_data$distributions <- table(c(rep(dat2$error_dist1,dat2$N_Response1),rep(dat2$error_dist2,dat2$N_Response2)))
summary_data$variable <- table(rep(dat2$variable2,dat2$N_Response_total))
summary_data$variable_cat <- table(rep(dat2$variable_cat,dat2$N_Response_total))
summary_data$variable_per_chick <- table(grepl("chick|adult",rep(dat2$variable2,dat2$N_Response_total)))


#### 
#--- Observation Periods
####


hist(dat2$obs_period_hours_mean, breaks=300, xlim=c(0,30), col="grey", main="Observation period")

dat_direct <- subset(dat2, direct=="direct")
dat_auto <- subset(dat2, direct=="automated")

obs_period_study <- aggregate(species~obs_period_hours_mean+ref,dat2,length)$obs_period_hours_mean
median(obs_period_study, na.rm=TRUE); range(obs_period_study, na.rm=TRUE)
quantile(obs_period_study, na.rm=TRUE,prob=0.80)

obs_period_direct <- aggregate(species~obs_period_hours_mean+ref,dat_direct,length)$obs_period_hours_mean
median(obs_period_direct, na.rm=TRUE); range(obs_period_direct, na.rm=TRUE)


####
#--- Mean number of observed visits
####

# if per chick, then scale mean back, and dont use sd
dat2$mean_rate_use <- ifelse(dat2$per_chick, dat2$mean_rate*dat2$mean_chicks_or_adults, dat2$mean_rate)
dat2$sd_rate_use <- ifelse(dat2$per_chick, NA, dat2$sd)

# scale mean and sd back to observed mean and sd using observation period
dat2$new_mean <- with(dat2, mean_rate_use * (obs_period_hours_mean/units_hours))
dat2$new_sd <- with(dat2, sd_rate_use * (obs_period_hours_mean/units_hours))

# repeat values for the number of analyses that were done
obsN_rep <- rep(dat2$new_mean, dat2$N_analysis)
hist(obsN_rep, breaks=30, col="grey");abline(v=median(obsN_rep, na.rm=TRUE),col="red")
summary_data$obs_mean <- c(n=sum(!is.na(obsN_rep)), median=median(obsN_rep, na.rm=TRUE),min=min(obsN_rep, na.rm=TRUE),max=max(obsN_rep, na.rm=TRUE))


####
#--- expected CVs
####

# calculate expected CV
dat2$exp_cv <- with(dat2, sqrt(new_sd^2 - new_mean)/new_mean)

# number where mean>variance
table(is.nan(dat2$exp_cv))

# for estiamtes where mean>variance, assign a value of 0
dat2$exp_cv_w0 <- ifelse(is.nan(dat2$exp_cv),0,dat2$exp_cv)

# repeat values for the number of analyses that were done
exp_cv_rep <- rep(dat2$exp_cv_w0, dat2$N_analysis)

summary_data$exp_cv <- c(n=sum(!is.na(exp_cv_rep)&!is.nan(exp_cv_rep)), median=median(exp_cv_rep,na.rm=TRUE), min=min(exp_cv_rep,na.rm=TRUE),max=max(exp_cv_rep,na.rm=TRUE), nan=sum(exp_cv_rep==0, na.rm=TRUE))

hist(exp_cv_rep, breaks=30, col="grey"); abline(v=median(exp_cv_rep,na.rm=TRUE),col="red")

#dat2[dat2$exp_cv>1 & !is.na(dat2$exp_cv),]

propExpVar <- function(mean,cv){
	exVar <- (mean*cv)^2
	obsVar <- mean+exVar
	return(exVar/obsVar)
}

dat2$prop <- with(dat2,propExpVar(new_mean,exp_cv))
dat2$prop_w0 <- with(dat2,propExpVar(new_mean,exp_cv_w0))
dat2$prop_minuses <- with(dat2,(new_sd^2 - new_mean) / new_sd^2) 

obs_prop <- rep(dat2$prop_w0, dat2$N_analysis)

summary_data$obs_prop <- c(n=sum(!is.na(obs_prop)&!is.nan(obs_prop)), median=median(obs_prop,na.rm=TRUE), min=min(obs_prop,na.rm=TRUE),max=max(obs_prop,na.rm=TRUE), nan=sum(obs_prop==0, na.rm=TRUE))


sapply(summary_data,sum)




save(summary_data, file="~/Dropbox/0_postdoc/8_PR repeat/shared/figures/summary_lit_review.Rdata")
with(dat2, tapply(new_mean, obs_method, median, na.rm=TRUE))
with(dat2, tapply(obs_period_hours_mean, obs_method, median, na.rm=TRUE))


24/sum(summary_data$distributions)
summary_data$distributions["Gaussian"]/sum(summary_data$distributions)

summary_data$variable_cat["number"]/sum(summary_data$variable_cat)


mm <- boxplot(new_mean ~ obs_method, dat2)
boxplot(new_mean ~ error_distribution1, dat2)
boxplot(new_mean ~ variable_cat, dat2)
boxplot(obs_period_hours_mean ~ obs_method, dat2[dat2$direct==1,])


nans <- subset(dat2, is.nan(exp_cv))[,c("ref", "obs_period_hours_mean", "mean_rate","new_mean", "sd", "new_sd", "prop_minuses", "units","units_hours","notes")]
nrow(nans)


save(dat2, file="~/Dropbox/0_postdoc/8_PR repeat/shared/figures/extracted_lit_review.Rdata")



