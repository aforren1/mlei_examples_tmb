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
  
  SIMULATE{
    for (i = 0; i < y.size(); i++) {
      y[i] = rbinom(n[i], invlogit(intercept + slope * x[i]));
    }
    REPORT(y);
  }
  return nll;
}
