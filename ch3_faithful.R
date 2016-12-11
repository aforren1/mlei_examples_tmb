library(TMB)

tmb_mod <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  PARAMETER(mu_1);
  PARAMETER(mu_2);
  PARAMETER(sd_1);
  PARAMETER(sd_2);
  PARAMETER(mix);
  Type nll = Type(0.0);
  int i;
  for (i = 0; i < y.size(); i++) {
  nll -= log(mix * dnorm(y[i], mu_1, exp(sd_1), false) + 
             (1.0 - mix) * dnorm(y[i], mu_2, exp(sd_2), false));
  }  
  return nll;
}
"

filename <- tempfile(tmpdir = '')
filedir <- tempdir()
write(tmb_mod, paste0(filedir, filename, '.cpp'))
compile(paste0(filedir, filename, '.cpp'))
dyn.load(dynlib(paste0(filedir, filename)))

data_list <- list(y = faithful$waiting)

# need to get in the ballpark, or it'll flail
params <- list(mu_1 = 80,
               sd_1 = 0,
               mu_2 = 50,
               sd_2 = 0, 
               mix = 0.5)

obj <- MakeADFun(data_list, params,
                 DLL = gsub("[^A-Za-z0-9]", "", filename))

low <- list(mu_1 = -100, mu_2 = -100, 
            sd_1 = -50, sd_1 = -50,
            mix = 0.001)
high <- list(mu_1 = 100, mu_2 = 100, 
            sd_1 = 50, sd_1 = 50,
            mix = 0.999)

opt <- nlminb(obj$par, obj$fn,
              obj$gr, lower = low,
              upper = high)

# likelihood surface of single parameters

prof_mix <- tmbprofile(obj, 'mix')
plot(prof_mix)
confint(prof_mix)
