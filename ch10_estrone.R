#setwd('~/R/mlei_book_tmb')

library(TMB)
library(lme4)

# NB: Different data from the book?

compile('estrone.cpp')
dyn.load(dynlib('estrone'))


estrone <- read.table('http://users.du.se/~lrn/StatMod12/Lab4/estrone.dat',
                      header = TRUE)

data_list <- list(measurement = estrone$measurement,
                  obs_per_id = c(table(estrone$id)))

params <- list(intercept = 20,
               log_sigma = 2,
               log_sigma_re = 1.2,
               u = rep(0, length(unique(estrone$id))))

obj <- MakeADFun(data_list, params, random = c('u'),
                 DLL = 'estrone')

# test simulate
plot(obj$simulate()$measurement)

opt <- do.call('optim', obj)

preds <- sdreport(obj)$value
  
ref_mod <- lmer(measurement ~ 1 + (1|id), data = estrone)