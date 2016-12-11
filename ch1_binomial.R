library(TMB)

tmb_mod <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_SCALAR(y);
  DATA_SCALAR(n);
  PARAMETER(p);

  Type nll = Type(0.0);
  nll -= dbinom(y, n, p, true);
  return nll;
}
"

filename <- tempfile(tmpdir = '')
filedir <- tempdir()
write(tmb_mod, paste0(filedir, filename, '.cpp'))
compile(paste0(filedir, filename, '.cpp'))
dyn.load(dynlib(paste0(filedir, filename)))

data_list <- list(y = 10, n = 100)

# need to get in the ballpark, or it'll flail
params <- list(p = 0.4)

obj <- MakeADFun(data_list, params,
                 DLL = gsub("[^A-Za-z0-9]", "", filename))

low <- list(p = 0)
high <- list(p = 1)

opt <- nlminb(obj$par, obj$fn,
              obj$gr, lower = low,
              upper = high)