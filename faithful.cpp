#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  PARAMETER(mu_1);
  PARAMETER(sd_1);
  PARAMETER(mu_2);
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