library(TMB)

compile('binomial.cpp')
dyn.load(dynlib('binomial'))

data_list <- list(y = 10, n = 100)

params <- list(p = 0.15)

obj <- MakeADFun(data_list, params,
                 DLL = 'binomial')

# trivial simulation
hist(replicate(10000, obj$simulate()$y/data_list$n), breaks = 50)


low <- list(p = 0)
high <- list(p = 1)

opt <- nlminb(obj$par, obj$fn,
              obj$gr, lower = low,
              upper = high)