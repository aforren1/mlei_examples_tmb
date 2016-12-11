#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(measurement);
  DATA_FACTOR(obs_per_id); // number of obs per subject
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
    
    for (j = 0; j < obs_per_id(i); j++) {
      nll -= dnorm(measurement[total], intercept + u[i], exp(log_sigma), true);
      total++;
    }
  }
  return nll;
}