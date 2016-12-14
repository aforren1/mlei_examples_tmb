#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_VECTOR(age);
  PARAMETER(beta1);
  PARAMETER(beta2);
  PARAMETER(beta3);
  
  PARAMETER(log_sigma);
  ADREPORT(exp(log_sigma));
  
  vector<Type> mu(y.size()); // predictions

  Type nll = Type(0.0);
  int i;
  for (i = 0; i < y.size(); i++) {
      nll -= dnorm(y[i], beta1 - beta2 * exp(-beta3 * age[i]), exp(log_sigma), true);
  }
  
  // predictions
  for (i = 0; i < y.size(); i++) {
    mu[i] = beta1 - beta2 * exp(-beta3 * age[i]);
  }
  ADREPORT(mu);
  
  SIMULATE {
    for (i = 0; i < y.size(); i++) {
      y[i] = rnorm(beta1 - beta2 * exp(-beta3 * age[i]), exp(log_sigma));
    }
    REPORT(y);
  }
  return nll;
}
