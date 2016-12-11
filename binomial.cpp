#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_SCALAR(y); // # successes
  DATA_SCALAR(n); // # observations
  PARAMETER(p);
  
  Type nll = Type(0.0);
  nll -= dbinom(y, n, p, true);
  return nll;
}