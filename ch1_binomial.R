library(TMB)

compile('binomial.cpp')
dyn.load(dynlib('binomial'))

data_list <- list(y = 10, n = 100)

params <- list(p = 0.4)

obj <- MakeADFun(data_list, params,
                 DLL = 'binomial')

low <- list(p = 0)
high <- list(p = 1)

opt <- nlminb(obj$par, obj$fn,
              obj$gr, lower = low,
              upper = high)