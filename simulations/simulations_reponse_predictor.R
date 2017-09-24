rm(list=ls())

library(rstan)
library(pbapply)
library(lme4)
setwd("~/Dropbox/0_postdoc/8_PR repeat/shared/online materials")

set.seed(25)


mean_obs <- (seq(1,10,length.out=200))^2 ## 1:100 ##exp(seq(0.5,4.75,length.out=100))
hist(mean_obs)


######
##----Datasets
######


datasets <- lapply(mean_obs, function(mean){
	N <- 100
	cv <- 0.3
	R2_PR <- 0.5
	sd <- mean*cv
	meanLat <- log(mean)
	varLat <- log(1+sd^2/mean^2)
	rain <- rnorm(N, 0, sqrt(R2_PR*varLat))
	lambda <- exp(rnorm(N,meanLat + rain,sqrt(varLat - R2_PR*varLat)))
	visits <- rpois(N,lambda)

	#simulate chick body mass
	sd_mass <- 20
	R2 <- 0.5
	beta_1 <- (sd_mass*sqrt(R2))/sd
	beta_0 <- 200 - beta_1*mean
	sigma <- sd_mass*sqrt(1-R2)
	mass <- rnorm(N, beta_0 + beta_1*lambda, sigma)

	data.frame(mass,visits,rain)
})



######
##----Response
######


R2_response <- as.data.frame(t(pbsapply(datasets, function(data){	
	mod <- lm(visits~rain,data)
	R2_lm <- var(model.matrix(mod)%*%coef(mod))/(var(model.matrix(mod)%*%coef(mod)) + summary(mod)$sigma^2)	

	data$obs <- 1:nrow(data)
	mod2 <- glmer(visits~rain + (1|obs),data,family="poisson")
	R2_pois <- var(model.matrix(mod2) %*% summary(mod2)$coef[,1])/(var(model.matrix(mod2) %*% summary(mod2)$coef[,1]) + as.numeric(summary(mod2)$var$obs))

	return(c(mean=mean(data$visits), R2_lm=R2_lm, R2_pois=R2_pois))
	})))
head(R2_response)

write.csv(R2_response,file="simulations/R2_response.csv",row.names=FALSE)




######
##----Predictor
######

stanModel <- stan_model(file = "simulations/Poisson_measurement_error_model.stan")


R2_predictor <- as.data.frame(t(pbsapply(datasets, function(data){	
	mod_LM <- lm(mass~visits, data)
	V_f_LM <- var(model.matrix(mod_LM) %*% coef(mod_LM))
	V_e_LM <- summary(mod_LM)$sigma^2
	R2_LM <- V_f_LM / (V_f_LM + V_e_LM)

	stanData <- list(N=nrow(data),a=data$mass,v=data$visits)
	fit <- sampling(stanModel, data = stanData, iter = 2000, chains = 1,open_progress = F, control = list(max_treedepth = 15, adapt_delta=0.91))
	ex_fit <- extract(fit, permute=FALSE)
	PR_pred <- ex_fit[,,grep("PR\\[",dimnames(ex_fit)[[3]])]
	outmat <- ex_fit[,,"beta_0"] + ex_fit[,,"beta_1"] * PR_pred[,]
	V_f_stan <- apply(outmat, 1, var)
	V_e_stan <- ex_fit[,,"sigma"]^2
	R2_stan <- V_f_stan / (V_f_stan + V_e_stan)
	
	return(c(mean=mean(data$visits), R2_lm=R2_LM, R2_mem=mean(R2_stan)))
})))

write.csv(R2_predictor,file="simulations/R2_predictor.csv",row.names=FALSE)




