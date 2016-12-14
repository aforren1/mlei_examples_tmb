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
  
 // ADREPORT(exp(log_sigma));
 // ADREPORT(exp(log_sigma_re));
  
  vector<Type> pred(measurement.size()); // predictions
  
  // likelihood & predictions
  Type nll = Type(0.0);
  int total, i, j;
  total = 0;
  for (i = 0; i < u.size(); i++) {
    nll -= dnorm(u[i], Type(0.0), exp(log_sigma_re), true);
    
    for (j = 0; j < obs_per_id(i); j++) {
      pred[total] = intercept + u[i];
      nll -= dnorm(measurement[total], intercept + u[i], exp(log_sigma), true);
      total++;
    }
  }
  // get stderrs for predictions & report
  ADREPORT(pred);
  
  SIMULATE{
    total = 0;
    for (i = 0; i < u.size(); i++) {
      u[i] = rnorm(Type(0.0), exp(log_sigma_re));
      for (j = 0; j < obs_per_id(i); j++) {
        measurement[total] = rnorm(intercept + u[i], exp(log_sigma));
        total++;
      }
    }
    REPORT(measurement);
  }
  return nll;
}
