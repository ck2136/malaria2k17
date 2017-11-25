rm(list = ls())

# Load Libraries

# install.packages("coefplot2", repos="http://www.math.mcmaster.ca/bolker/R", type="source")
# install.packages("reshape")

library(MCMCglmm) # Bayesian Approach
library(MASS) # for trying glmmPQL... 
library(lme4) # for trying glmer... doesn't work
library(EpiWeek) # for adding time
library(parallel) # for checking MCMC gelman-rubin criterion
library(coda) # plotting gelman-rubin criterion plot
library(arm) # for coefplots
library(rstan) # MCMCusing stan
library(brms) # for glmm MCMC
library(rstanarm) # for glmm MCMC
library(future) # for parallelizing brms 
library(pander) # for table
library(sjPlot) # for glmer plotting
library(splines) # spline term for time

# Load dataset
final <- readRDS("~/Documents/PythonR/Project/data/final.rds")
# Add time
final$time <- as.numeric(difftime(epiweekToDate(final$Epiyear, final$Epiweek)[[1]], range(epiweekToDate(final$Epiyear, final$Epiweek)[[1]])[1], units = c("weeks"))/52.25)
# set District as factor
final$District <- as.factor(final$District)
load("appendix.RData")
load("~/Documents/PythonR/Project/data/appendix.RData")

###############
# Descriptives#
###############

# 1. check the counts of interventions that were provided by region and province
final %>%
  select(DISTCODE, Region, IRSprotn) %>%
  group_by(Region) %>%
  filter(IRSprotn == 1) %>%
  summarise(count = n())

final %>%
  select(DISTCODE, Region, ITNprotn) %>%
  group_by(Region) %>%
  filter(ITNprotn == 1) %>%
  summarise(count = n())


final %>%
  select(DISTCODE, Province, IRSprotn) %>%
  group_by(Province) %>%
  filter(IRSprotn == 1) %>%
  summarise(count = n())


# 2. Incidence of Malaria

# districts that had > 100 incidence and the count from 2010 to 2017 
final %>%
  select(DISTCODE, Epiyear, Province, District, inc1k) %>%
  group_by(District, Epiyear) %>%
  summarise(inc1k = mean(inc1k)) %>%
  filter(inc1k > 100) %>%
  summarise(count = n())

final %>%
  select(DISTCODE, Epiyear, Province, District, inc1k) %>%
  group_by(District, Epiyear) %>%
  summarise(inc1k = mean(inc1k)) %>%
  filter(inc1k > 100) %>%
  summarise(inc1k = mean(inc1k))



#####################
# Poisson Regression#
#####################

# Have used glmmadmb, glmer.nb but it seems that going the bayesian route is appropriate since the frequentist versions don't play well
# glmmadmb and glmer.nb require the data to be complete cases
# gonna produce PQL, glmer with warning, and MCMCglmm 3 models

## Model 1: Regression with only intervention variables

# PQL Pseudo-quasi likelihood
gm1 <- glmmPQL(cases ~ scale(ITNprotn) + scale(IRSprotn) + offset(log(u5total)) +  time, random = ~1|District,  family = poisson, data = final)

# Adaptive Gaussian Quadrature
gm1a <- glmer(cases ~ scale(ITNprotn) + scale(IRSprotn) + offset(log(u5total)) + (1|District) + time,  data = final, nAGQ = 25) # need complete cases... maybe inputation for those missing?

# MCMC
gm1mcmc <- MCMCglmm(cases ~ scale(ITNprotn) + scale(IRSprotn) + offset(log(u5total)) +  time, random = ~District,  family = "poisson", data = final)


## Model 2: Regression with only weather variables

### Model 2a: Regression with only weather variables 0 weeks lag
gm2a <- glmer(cases ~ scale(tavg) + scale(raintot) + scale(rh) + scale(sd) + scale(psfc) + offset(log(u5total)) + (time|District) + time, family = poisson, data = final)

### Model 2b: Regression with only weather variables 2 weeks lag
gm2b <- glmer(cases ~ scale(tavg_lag_2) + scale(raintot_lag_2) + scale(rh_lag_2) + scale(sd_lag_2) + scale(psfc_lag_2) + offset(log(u5total)) + (time|District) + time, family = poisson, data = final)

### Model 2c: Regression with only weather variables 4 weeks lag
gm2c <- glmer(cases ~ scale(tavg_lag_4) + scale(raintot_lag_4) + scale(rh_lag_4) + scale(sd_lag_4) + scale(psfc_lag_4) + offset(log(u5total)) + (time|District) + time family = poisson, data = final)

### Model 2d: Regression with only weather variables 8 weeks lag
gm2d <- glmer(cases ~ scale(tavg_lag_4) + scale(raintot_lag_4) + scale(rh_lag_4) + scale(sd_lag_4) + scale(psfc_lag_4) + offset(log(u5total)) + (time|District) + time family = poisson, data = final)



## Model 3: Regression with both intervention and  weather variables # most likely what will go into the final restuls output

# When we don't set priors... (which we shouldn't) the AR is about 0.19 close to 0.234 so ok
# Probably should try thinning so thin = 20 or 30 and inc niter to 23000 and 33000
# Also offset doesn't work in MCMCglmm so need to fit Obstime as covariate then fix associated regression coefficient to 1 in the prior...


prior <- list(
  B = list(V = diag(12)*1e7, mu =c(0,0,0,0,0,0,0,0,1,0,0,0)), # fix the beta value for the log(u5total) to be 1 s.t. it acts as an offset
  R = list(V = 1, n = 1, fix = 1),
  G = list(
    G1 = list(V = 1, n = 1)
  ))
prior$B$V[9,9] <- 1e-7 # replace u5total (offset) variance to 0 so that it's stuck at 0
prior # check the matrices


### Model 3a: Regression with both intervention and weather variables no lag

gm3a <- glmer(cases ~ scale(ITNprotn) + scale(IRSprotn) + scale(tavg) + scale(raintot) + scale(rh) + scale(sd) + scale(psfc) + offset(log(u5total)) + (1|District) + scale(ITNprotn)*ns(time, df=3) + scale(IRSprotn)*ns(time, df=3) + ns(time, df=3), family = poisson, data = final)

gm3amc <- mclapply(1:4, function(i) {
  MCMCglmm(cases ~ 
             scale(ITNprotn) + scale(IRSprotn) + scale(tavg) + scale(raintot) + scale(rh) + scale(sd) + scale(psfc) + log(u5total) + scale(ITNprotn)*time + scale(IRSprotn)*time + time,
           random = ~District, family = "poisson", data = final[complete.cases(final),],
           prior = prior)
  # prior  = prior.m5,
  # thin   = 20,
  # burnin = 3000,
  # nitt   = 23000)
}, mc.cores=4)



### Model 3b: Regression with both intervention and weather variables 2 weeks lag
gm3b <- glmer(cases ~ scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_2) + scale(raintot_lag_2) + scale(rh_lag_2) + scale(sd_lag_2) + scale(psfc_lag_2) + offset(log(u5total)) + (1|District) + scale(ITNprotn)*ns(time, df=3) + scale(IRSprotn)*ns(time, df=3) + ns(time, df=3), family = poisson, data = final)

gm3bmc <- mclapply(1:4, function(i) {
  MCMCglmm(cases ~ 
             scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_2) + scale(raintot_lag_2) + scale(rh_lag_2) + scale(sd_lag_2) + scale(psfc_lag_2) + log(u5total) + scale(ITNprotn)*time + scale(IRSprotn)*time + time,
           random = ~District, family = "poisson", data = final[complete.cases(final),],
           prior = prior)
  # prior  = prior.m5,
  # thin   = 20,
  # burnin = 3000,
  # nitt   = 23000)
}, mc.cores=4)




### Model 3c: Regression with both intervention and weather variables 4 weeks lag
gm3c <- glmer(cases ~ scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_4) + scale(raintot_lag_4) + scale(rh_lag_4) + scale(sd_lag_4) + scale(psfc_lag_4) + offset(log(u5total)) + (1|District) + scale(ITNprotn)*ns(time, df=3) + scale(IRSprotn)*ns(time, df=3) + ns(time, df=3), family = poisson, data = final)

gm3cmc <- mclapply(1:4, function(i) {
  MCMCglmm(cases ~ 
             scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_4) + scale(raintot_lag_4) + scale(rh_lag_4) + scale(sd_lag_4) + scale(psfc_lag_4) + log(u5total) + scale(ITNprotn)*time + scale(IRSprotn)*time + time,
           random = ~District, family = "poisson", data = final[complete.cases(final),],
           prior = prior)
  # prior  = prior.m5,
  # thin   = 20,
  # burnin = 3000,
  # nitt   = 23000)
}, mc.cores=4)


### Model 3c: Regression with both intervention and weather variables 8 weeks lag
gm3d <- glmer(cases ~ 
                scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_8) + scale(raintot_lag_8) +  scale(rh_lag_8) + scale(sd_lag_8) + scale(psfc_lag_8) + offset(log(u5total)) + (1|District) + scale(ITNprotn)*ns(time, df=3) + scale(IRSprotn)*ns(time, df=3) + ns(time, df=3), 
              family = poisson, data = final ) #optimx nlminb method

gm3dmc <- mclapply(1:4, function(i) {
  MCMCglmm(cases ~ 
             scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_8) + scale(raintot_lag_8) + scale(rh_lag_8) + scale(sd_lag_8) + scale(psfc_lag_8) + log(u5total) + scale(ITNprotn)*time + scale(IRSprotn)*time + time,
           random = ~District, family = "poisson", data = final[complete.cases(final),],
           prior = prior)
  # prior  = prior.m5,
  # thin   = 20,
  # burnin = 3000,
  # nitt   = 23000)
}, mc.cores=4)





#####################
# Regression Results# 
#####################

# GLMER models plot
plot_models(gm3a, gm3b, gm3c, gm3d, std.est = "std2", 
            legend.title = "Model Numbers", 
            m.labels = c("Model 1", "Model 2", "Model 3","Model 4")
            )

# MCMC models 
save_mod <- function(y) {
  m1 <- lapply(get(y), function(m) m$Sol)
  m1 <- do.call(mcmc.list, m1)
  return(m1)
}


# all chains stored here first 1-4 is nolag, then 2, 4, and then 8 week lags
models <- lapply(ls()[grepl("mc$",ls())], save_mod)
summary(models[[1]])

plot_mcmod <- function(y) {
  
  # extract each of the MCMC models from list and get parameter estimates
  x <- summary(y[[1]])
  x2 <- summary(y[[2]])
  x3 <- summary(y[[3]])
  x4 <- summary(y[[4]])
  
  # creating plot scheme
  n <- dim(x$statistics)[1]
  par(mar=c(4, 10, 4, 1))
  plot(x$statistics[,1], n:1,
       yaxt="n", ylab="", xlab = "Parameter estimates",
       xlim=range(x$quantiles)*1.2,
       pch=19,
       main="Posterior means and 95% credible intervals", col=c("red"))
  points(x2$statistics[,1], n:1, pch=19, col = "blue")
  points(x3$statistics[,1], n:1, pch=19, col = "green")
  points(x4$statistics[,1], n:1, pch=19, col = "yellow")
  grid()
  axis(2, at=n:1, rownames(x$statistics) <- c("Intercept", "ITN", "IRS", "Avg. Temp (C)", "Avg Rain (mm/week)", "Rel. Humidity (%)", "Vapor Pressure (mmHg)","Surface Pressure (hPa)", "Under 5 Total", "Time", "ITN:Time", "IRS:Time") , las=2) # originally rownames(x$statistics)
  arrows(x$quantiles[,1], n:1, x$quantiles[,5], n:1, code=0)
  abline(v=0, lty=2)
  legend("bottomleft", legend = c("Model 1", "Model 2", "Model 3", "Model 4"), lty = c(1,2,3,4), col = c("red","blue","green","yellow"))
}

plot_mcmod(models)

save.image(file='appendix.RData') # save the data to the environment to input into the shiny document
save("models", file = "analysis.RData")


### Table of the 4 models
resultab<- cbind(summary(models[[1]])$statistics[,1:2], summary(models[[2]])$statistics[,1:2], summary(models[[3]])$statistics[,1:2], summary(models[[4]])$statistics[,1:2])
rownames(resultab) <- c("Intercept", "ITN", "IRS", "Avg. Temp (C)", "Avg Rain (mm/week)", "Rel. Humidity (%)", "Vapor Pressure (mmHg)","Surface Pressure (hPa)", "Under 5 Total", "Time", "ITN:Time", "IRS:Time")
colnames(resultab) <- c("M1 mean", "M1 SD", "M2 mean", "M2 SD", "M3 mean", "M3 SD", "M4 mean", "M4 SD")
pander(resultab)


exp(summary(models[[4]])$statistics[,1][4]) # tavg
exp(summary(models[[4]])$statistics[,1][6]) # rel humi
exp(summary(models[[4]])$statistics[,1][7]) # vap pres


# Another way to get coefficient estimates plotted
library(coefplot2)
coefplot2(list("MCMC 1" = models[[1]],
               "MCMC 2" = models[[2]],
               "MCMC 3" = models[[3]],
               "MCMC 4" = models[[4]],
               "GLMER 1" = gm3a,
               "GLMER 2" = gm3b,
               "GLMER 3" = gm3c,
               "GLMER 4" = gm3d
               
               ), merge.names = FALSE,
          intercept=TRUE,
          legend=TRUE,
          legend.x = "left",
          varnames =  c("Intercept", "ITN", "IRS", "Avg. Temp (C)", "Avg Rain (mm/week)", "Rel. Humidity (%)", "Vapor Pressure (mmHg)","Surface Pressure (hPa)", "Under 5 Total", "Time", "ITN:Time", "IRS:Time")
)

summary(gm3a)
coefplot2(list("GLMER 1" = gm3a,
            "GLMER 2" = gm3b,
            "GLMER 3" = gm3c,
            "GLMER 4" = gm3d),  merge.names = FALSE,
     intercept=TRUE,
     legend=TRUE,
     legend.x = "left")

coefplot2(list())


####################
#### MCMC DIAGNOSES#
####################



## Autocorrelation plot
plot.acfs <- function(x) {
  n <- dim(x)[2]
  par(mfrow=c(ceiling(n/6),2), mar=c(3,2,3,0))
  for (i in 1:n) {
    acf(x[,i], lag.max=100, main=colnames(x)[i])
    grid()
  }
}
plot.acfs(gm3dmc[[1]]$Sol)
par(mfrow=c(2,2))
gelman.plot(gm3dmcmix, auto.layout = F)
gelman.diag(gm3dmcmix)
plot(gm3dmcmix, ask=F, auto.layout=T)

# Check Trace for Random Effects
print(xyplot(as.mcmc(gm3dmc[[1]]$VCV), layout=c(2,1)))
print(xyplot(as.mcmc(gm3dmc[[2]]$VCV), layout=c(2,1)))
print(xyplot(as.mcmc(gm3dmc[[3]]$VCV), layout=c(2,1)))
print(xyplot(as.mcmc(gm3dmc[[4]]$VCV), layout=c(2,1)))


# plotting estimates function for diagnosis

plot.estimates <- function(x) {
  if (class(x) != "summary.mcmc")
    x <- summary(x)
  n <- dim(x$statistics)[1]
  par(mar=c(2, 10, 4, 1))
  plot(x$statistics[,1], n:1,
       yaxt="n", ylab="",
       xlim=range(x$quantiles)*1.2,
       pch=19,
       main="Posterior means and 95% credible intervals", col=c("red"))
  grid()
  axis(2, at=n:1, rownames(x$statistics) <- c("Intercept", "ITN", "IRS", "Avg. Temp (C)", "Avg Rain (mm/week)", "Rel. Humidity (%)", "Vapor Pressure (mmHg)","Surface Pressure (hPa)", "Under 5 Total", "Time", "ITN:Time", "IRS:Time") , las=2) # originally rownames(x$statistics)
  arrows(x$quantiles[,1], n:1, x$quantiles[,5], n:1, code=0)
  abline(v=0, lty=2)
}

par(mfrow=c(1,1))
plot.estimates(gm3dmcmix) # estimate using all 4 parallel MCMC's
summary(gm3dmcmix)$statistics










##################
# Other Packages #
##################

# MCMC using brms and rstanarm


## Start with brms
plan(multiprocess)
fit <- brm(cases ~ 
             scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_8) + 
             scale(raintot_lag_8) + scale(rh_lag_8) + scale(sd_lag_8) + 
             scale(psfc_lag_8) + offset(log(u5total)) + scale(ITNprotn)*time +  
             scale(IRSprotn)*time + time + (1|DISTCODE), 
           data = final[complete.cases(final),], family = "poisson")
### Using rstanarm

fit <- stan_glmer(cases ~ 
                    scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_8) + 
                    scale(raintot_lag_8) + scale(rh_lag_8) + scale(sd_lag_8) + 
                    scale(psfc_lag_8) + offset(log(u5total)) + scale(ITNprotn)*time +  
                    scale(IRSprotn)*time + time + (1|DISTCODE),
                    data = final[complete.cases(final),], family = poisson(link="log"),
                    QR = TRUE, prior = normal(0, 1), prior_intercept = normal(-3, 1),
                    chains = 3, cores = 3, seed = 1234, iter = 200)

summary(fit)
plot(fit)
## Data Preparation

## Stan Code

stanmodelcode <- ' 
data {
int<lower=0> Nobs; // sample size
int<lower=0> Nsubs; // here subject will be district
int<lower=0> Npreds;
int<lower=0,upper=1> y[Nobs];
int<lower=1,upper=Nsubs> subject[Nobs];
matrix[Nobs,Npreds] x;
vector[Nobs] offset; 
}
parameters {
vector[Nsubs] subeff;
real<lower=0> sigmasubj;
vector[Npreds] beta;
}
model {
subeff ~ normal(0,sigmasubj);
sigmasubj ~ cauchy(0, 1);
for(n in 1:Nobs) {
y[n] ~ poisson_log(log(offset[n])+x[n]*beta + subeff[subject[n]] );
}
}
'
xm <- model.matrix( ~ scale(ITNprotn) + scale(IRSprotn) + scale(tavg_lag_8) + scale(raintot_lag_8) + scale(rh_lag_8) + scale(sd_lag_8) + scale(psfc_lag_8) + time*scale(ITNprotn) + time*scale(IRSprotn), final[complete.cases(final),])
finalst <- with(final[complete.cases(final),],
                list(Nobs=nrow(final[complete.cases(final),]), 
                     Nsubs=length(unique(DISTCODE)),
                           Npreds=ncol(xm),
                           y=cases,
                           subject=DISTCODE,
                           x=xm, 
                           offset=u5total))


rstan_options(auto_write = TRUE) # set stan to write auto
options(mc.cores = parallel::detectCores()) # use parallel to run the 3 chains

fit3dmc = stan(model_code=stanmodelcode, data=finalst, iter=12000, # 12000 iterations 
           warmup=2000, thin=10, chains=3) # burnin 2000 iterations, take 10 per draw and run 3 chains




# References

## MCMCglmm offsett doesn't work
#https://hannahdugdale.wordpress.com/2016/03/16/why-you-shouldnt-use-the-offset-function-in-mcmcglmm/ 

## GLMM overall
#https://rpubs.com/bbolker/glmmchapter 

# Multiple Optimizers
# references https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html 

# Plotting
## https://cran.r-project.org/web/packages/sjPlot/vignettes/sjplmer.html
## https://www.r-statistics.com/2010/07/visualization-of-regression-coefficients-in-r/ 


# Tables for Rmarkdown
# https://stackoverflow.com/questions/24342162/regression-tables-in-markdown-format-for-flexible-use-in-r-markdown-v2

# rstan information

# https://groups.google.com/forum/#!topic/stan-users/aqaIJlop33U

# rstan related library installation
#install.packages("rstan", repos = "https://cloud.r-project.org/")
#install.packages("shinystan", repos = "https://cloud.r-project.org/")
#install.packages("brms", repos = "https://cloud.r-project.org/")
#install.packages("rstanarm", repos = "https://cloud.r-project.org/")
