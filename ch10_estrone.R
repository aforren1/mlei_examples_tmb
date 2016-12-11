library(TMB)
library(lme4)

# NB: Different data from the book?

tmb_mod <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_FACTOR(obs); // number of observations per subject
  PARAMETER(intercept);

  PARAMETER(log_sigma);
  PARAMETER(log_sigma_re);
  PARAMETER_VECTOR(u);

  ADREPORT(exp(log_sigma));
  ADREPORT(exp(log_sigma_re));
  
  Type nll = Type(0.0);
  int total, i, j;
  total = 0;
  for (i = 0; i < u.size(); i++) {
    nll -= dnorm(u[i], Type(0.0), exp(log_sigma_re), true);
    
    for (j = 0; j < obs(i); j++) {
      nll -= dnorm(y[total], intercept + u[i], exp(log_sigma), true);
      total++;
    }
  }
  return nll;
}
"



filename <- '/estrone'
filedir <- tempdir()
write(tmb_mod, paste0(filedir, filename, '.cpp'))
compile(paste0(filedir, filename, '.cpp'))
dyn.load(dynlib(paste0(filedir, filename)))


estrone <- read.table('http://users.du.se/~lrn/StatMod12/Lab4/estrone.dat',
                      header = TRUE)

data_list <- list(y = estrone$measurement,
                  obs = ave(estrone$measurement, 
                            estrone$id, 
                            FUN = function(x) length(unique(x))))

# need to get in the ballpark, or it'll flail
params <- list(intercept = 0,
               log_sigma = 0,
               log_sigma_re = 0,
               u = rep(0, length(unique(estrone$id))))

obj <- MakeADFun(data_list, params, random = c('u'),
                 DLL = substring(filename, 2))

opt <- do.call('optim', obj)

ref_mod <- lmer(measurement ~ 1 + (1|id), data = estrone)