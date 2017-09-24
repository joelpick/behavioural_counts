rm(list=ls())

setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")



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

dat <- read.csv(file="lit_review/lit_review_data_new.csv",stringsAsFactors=FALSE)
names(dat)
nrow(dat)

####
#--- Study Level Metrics
####

## obs method as binary
dat$direct <- dat$obs_method %in% c("direct","video","mixed direct + video")

## clean data availability
dat$data_available2 <- grepl("TRUE",dat$data_available)


summary_data <- list(
	studies = length(unique(dat$ref)),
	obs_method = table(aggregate(species~obs_method+ref,dat,length)$obs_method), 
	obs_method_bin = table(aggregate(species~direct+ref,dat,length)$direct),
	single_obs_period = table(aggregate(species~single_obs_period_length+ref,dat,length)$single_obs_period_length),
	data_available = table(aggregate(species~data_available2+ref,dat,length)$data_available2),
	datasets = nrow(dat)
)


####
#--- Species Names
####

## split up species in studies with two species
dat$species1 <- sapply(strsplit(dat$species,"/ "), function(x) x[1])
dat$species2 <- sapply(strsplit(dat$species,"/ "), function(x) x[2])

## extract species name
dat$scientific_name1 <- gsub("\\(","",gsub("\\)","",sapply(strsplit(dat$species1,"\ "), function(x) pasteV(x[(length(x)-1):length(x)]))))

dat$scientific_name2 <- gsub("\\(","",gsub("\\)","",sapply(strsplit(dat$species2,"\ "), function(x) pasteV(x[(length(x)-1):length(x)]))))

dat$scientific_name <- ifelse(dat$scientific_name2=="NA", dat$scientific_name1, paste(dat$scientific_name1,dat$scientific_name2, sep=" / "))


## extract common name
## seperate out common name, then remove any commas, then remoe any trailing 's' and then capitalise words

dat$common_name1 <- sapply(remove_trailing_s(gsub(",","",sapply(strsplit(dat$species1,"\ "), function(x) pasteV(x[1:(length(x)-2)])))),simpleCap)

dat$common_name2 <- gsub(",","", sapply(strsplit(dat$species2, "\ "), function(x) ifelse(is.na(x[1]), NA, simpleCap(remove_trailing_s(pasteV(x[1:(length(x)-2)]))))))

dat$common_name <- ifelse(is.na(dat$common_name2), dat$common_name1, paste(dat$common_name1,dat$common_name2, sep=" / "))

summary_data$species <- length(unique(c(dat$scientific_name1,dat$scientific_name2)))
length(unique(c(dat$common_name1,dat$common_name2))) == length(unique(c(dat$scientific_name1,dat$scientific_name2)))



#### 
#--- Methods
####

## analyses
dat$N_Response1 <- as.numeric(sapply(strsplit(dat$N_Response,"/"), function(x) if(is.na(x[1])) 0 else x[1]))
dat$N_Response2 <- as.numeric(sapply(strsplit(dat$N_Response,"/"), function(x) if(is.na(x[2])) 0 else x[2]))
dat$N_Response_total <- dat$N_Response1 + dat$N_Response2
dat$N_analysis <- apply(dat[,c("N_Predictor","N_Response_total")],1,sum, na.rm=TRUE)

## analysis method
## LM, LMM, GLM, GLMM, non-parametric
## LM inclused regression, t-test, anova, correlation
dat$method1 <- sapply(strsplit(dat$methods,"/"), function(x) x[1])
dat$method2 <- sapply(strsplit(dat$methods,"/"), function(x) x[2])

## error dstribution used
## Gaussian, Poisson, Negative Binomial, Quasi-poisson, zero-inflated Poisson, gamma, non-parametric
dat$error_dist1 <- sapply(strsplit(dat$error_distribution,"/"), function(x) x[1])
dat$error_dist2 <- sapply(strsplit(dat$error_distribution,"/"), function(x) x[2])

## variable analysed
dat$variable2 <- sapply(strsplit(dat$variable,"/"), function(x){
	x[grepl("hour",x)|grepl("min",x)|grepl("day",x)] <- "time"
	pasteV(x, sep="/")
	})


summary_data$analyses <- apply(dat[,c("N_Response_total","N_Predictor")],2,sum, na.rm=TRUE)
summary_data$methods <- table(c(rep(dat$method1,dat$N_Response1),rep(dat$method2,dat$N_Response2)))
summary_data$distributions <- table(c(rep(dat$error_dist1,dat$N_Response1),rep(dat$error_dist2,dat$N_Response2)))
summary_data$variable <- table(rep(dat$variable2,dat$N_Response_total))
summary_data$variable_cat <- table(rep(dat$variable_cat,dat$N_Response_total))
summary_data$variable_per_chick <- table(grepl("chick|adult",rep(dat$variable2,dat$N_Response_total)))


#### 
#--- Observation Periods
####


hist(dat$obs_period_hours_mean, breaks=300, xlim=c(0,30), col="grey", main="Observation period")

dat_direct <- subset(dat, direct=="direct")
dat_auto <- subset(dat, direct=="automated")

obs_period_study <- aggregate(species~obs_period_hours_mean+ref,dat,length)$obs_period_hours_mean
median(obs_period_study, na.rm=TRUE); range(obs_period_study, na.rm=TRUE)
quantile(obs_period_study, na.rm=TRUE,prob=0.80)

obs_period_direct <- aggregate(species~obs_period_hours_mean+ref,dat_direct,length)$obs_period_hours_mean
median(obs_period_direct, na.rm=TRUE); range(obs_period_direct, na.rm=TRUE)


####
#--- Mean number of observed visits
####

# if per chick, then scale mean back, and dont use sd
dat$mean_rate_use <- ifelse(dat$per_chick, dat$mean_rate*dat$mean_chicks_or_adults, dat$mean_rate)
dat$sd_rate_use <- ifelse(dat$per_chick, NA, dat$sd)

# scale mean and sd back to observed mean and sd using observation period
dat$new_mean <- with(dat, mean_rate_use * (obs_period_hours_mean/units_hours))
dat$new_sd <- with(dat, sd_rate_use * (obs_period_hours_mean/units_hours))

# repeat values for the number of analyses that were done
obsN_rep <- rep(dat$new_mean, dat$N_analysis)
hist(obsN_rep, breaks=30, col="grey");abline(v=median(obsN_rep, na.rm=TRUE),col="red")
summary_data$obs_mean <- c(n=sum(!is.na(obsN_rep)), median=median(obsN_rep, na.rm=TRUE),min=min(obsN_rep, na.rm=TRUE),max=max(obsN_rep, na.rm=TRUE))


####
#--- expected CVs
####

# calculate expected CV
dat$exp_cv <- with(dat, sqrt(new_sd^2 - new_mean)/new_mean)

# number where mean>variance
table(is.nan(dat$exp_cv))

# for estiamtes where mean>variance, assign a value of 0
dat$exp_cv_w0 <- ifelse(is.nan(dat$exp_cv),0,dat$exp_cv)

# repeat values for the number of analyses that were done
exp_cv_rep <- rep(dat$exp_cv_w0, dat$N_analysis)

summary_data$exp_cv <- c(n=sum(!is.na(exp_cv_rep)&!is.nan(exp_cv_rep)), median=median(exp_cv_rep,na.rm=TRUE), min=min(exp_cv_rep,na.rm=TRUE),max=max(exp_cv_rep,na.rm=TRUE), nan=sum(exp_cv_rep==0, na.rm=TRUE))

hist(exp_cv_rep, breaks=30, col="grey"); abline(v=median(exp_cv_rep,na.rm=TRUE),col="red")

#dat[dat$exp_cv>1 & !is.na(dat$exp_cv),]

propExpVar <- function(mean,cv){
	exVar <- (mean*cv)^2
	obsVar <- mean+exVar
	return(exVar/obsVar)
}

dat$prop <- with(dat,propExpVar(new_mean,exp_cv))
dat$prop_w0 <- with(dat,propExpVar(new_mean,exp_cv_w0))
dat$prop_minuses <- with(dat,(new_sd^2 - new_mean) / new_sd^2) 

obs_prop <- rep(dat$prop_w0, dat$N_analysis)

summary_data$obs_prop <- c(n=sum(!is.na(obs_prop)&!is.nan(obs_prop)), median=median(obs_prop,na.rm=TRUE), min=min(obs_prop,na.rm=TRUE),max=max(obs_prop,na.rm=TRUE), nan=sum(obs_prop==0, na.rm=TRUE))


sapply(summary_data,sum)




save(summary_data, file="lit_review/summary_lit_review.Rdata")
with(dat, tapply(new_mean, obs_method, median, na.rm=TRUE))
with(dat, tapply(obs_period_hours_mean, obs_method, median, na.rm=TRUE))


24/sum(summary_data$distributions)
summary_data$distributions["Gaussian"]/sum(summary_data$distributions)
summary_data$variable_cat["number"]/sum(summary_data$variable_cat)


mm <- boxplot(new_mean ~ obs_method, dat)
boxplot(new_mean ~ error_dist1, dat)
boxplot(new_mean ~ variable_cat, dat)
boxplot(obs_period_hours_mean ~ obs_method, dat[dat$direct==1,])


nans <- subset(dat, is.nan(exp_cv))[,c("ref", "obs_period_hours_mean", "mean_rate","new_mean", "sd", "new_sd", "prop_minuses", "units","units_hours","notes")]
nrow(nans)


save(dat, file="lit_review/extracted_lit_review.Rdata")


names(dat)

dat_out <- dat[order(dat$ref),c("ref", "scientific_name", "common_name", "N_Predictor", "N_Response", "N_analysis", "variable2", "variable_cat", "error_distribution", "data_available", "obs_method", "obs_period_hours_mean", "single_obs_period_length", "pooled", "pooled_groups", "units", "units_hours","per_chick", "mean_chicks_or_adults", "mean_rate","sd", "new_mean", "new_sd", "exp_cv_w0", "prop_w0", "notes")]

names(dat_out) <- c("Ref", "Scientific_Name", "Common_Name", "N_Predictor", "N_Response", "N_Analysis", "Variable", "Variable_Category", "Error_Distribution", "Data_Available", "Observation_Method", "Mean_Observation_Period_hours", "Single_Observation_Period?", "Pooled_Estimates?", "N_Pooled_Groups", "Estimate_Units", "Estimate_Units_hours", "Per_Chick", "Mean_N_Chicks", "Mean_Rate","SD_Rate", "Adjusted_Mean_Rate", "Adjusted_SD_Rate", "Expexted_CV", "Proportion_Expected_V", "Notes")

head(dat_out)
write.csv(dat_out, file="lit_review/lit_review_for_S.csv", row.names=FALSE)

dat$N_Response
