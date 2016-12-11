# this example is a bit touchy (re: starting values, profiling)
library(TMB)

compile('faithful.cpp')
dyn.load(dynlib('faithful'))

data_list <- list(y = faithful$waiting)

# need to get in the ballpark, or it'll flail
# keep in mind that sd_* are log-transformed (need to exp() to get value)
params <- list(mu_1 = 80,
               sd_1 = 1,
               mu_2 = 50,
               sd_2 = 1, 
               mix = .5) 

obj <- MakeADFun(data_list, params,
                 DLL = 'faithful')

low <- list(mu_1 = -100, mu_2 = -100, 
            sd_1 = -100, sd_1 = -100,
            mix = 0)
high <- list(mu_1 = 100, mu_2 = 100, 
            sd_1 = 100, sd_1 = 100,
            mix = 1)

opt <- nlminb(obj$par, obj$fn,
              obj$gr, lower = low,
              upper = high)

# likelihood surface of single parameters 

prof_mix <- tmbprofile(obj, 'mix')
plot(prof_mix)
confint(prof_mix)

# MCMC sampling on top (doesn't do very well w/ current settings)

nuts <- run_mcmc(obj, nsim = 500, algorithm = 'NUTS', 
                 params.init = opt$par, diagnostic = TRUE, 
                 eps = 0.01)

plot(nuts$par[,5], type = 'l')
coda::HPDinterval(as.mcmc(nuts$par[,5]))