library(TMB)

tmb_mod <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_VECTOR(n);
  DATA_VECTOR(x);
  PARAMETER(intercept);
  PARAMETER(slope);
  
  Type nll = Type(0.0);
  int i;
  for (i = 0; i < y.size(); i++) {
    nll -= dbinom(y[i], n[i], invlogit(intercept + slope * x[i]), true);
  }
  return nll;
}
"

filename <- tempfile(tmpdir = '')
filedir <- tempdir()
write(tmb_mod, paste0(filedir, filename, '.cpp'))
compile(paste0(filedir, filename, '.cpp'))
dyn.load(dynlib(paste0(filedir, filename)))


haddock <- read.table(paste0('https://www.stat.auckland.ac.nz/', 
                       '~millar/MLEI/Data/Clark113_20min.dat'), 
                      header = TRUE)
data_list <- list(y = haddock$codend, 
                  n = haddock$codend + haddock$cover,
                  x = haddock$lenclass)

# need to get in the ballpark, or it'll flail
params <- list(intercept = 0,
               slope = 0)

obj <- MakeADFun(data_list, params,
                 DLL = gsub("[^A-Za-z0-9]", "", filename))

low <- list(p = 0)
high <- list(p = 1)

opt <- do.call('optim', obj)

print(summary(sdreport(obj)))

comp_glm <-glm(codend/I(codend + cover) ~ lenclass, 
               data = haddock, family = 'binomial', 
               weights = codend + cover)

print(summary(comp_glm)$coefficients)