# von-Bertalanffy growth curve
library(TMB)
vonB <- read.table(text = "  1        7.36
  2       14.3
  3       21.8
  4       27.6
  5       31.5
  6       35.3
  7       39.0
  8       41.1
  9       43.8
  10      45.1
  11      47.4
  12      48.9
  13      50.1
  14      51.7
  15      51.7
16 54.1")

names(vonB) <- c('age', 'y')

compile('vonb.cpp')
dyn.load(dynlib('vonb'))

data_list <- list(y = vonB$y,
                  age = vonB$age)


params <- list(beta1 = 57,
               beta2 = 58,
               beta3 = .16,
               log_sigma = -1) 

obj <- MakeADFun(data_list, params,
                 DLL = 'vonb')

opt <- do.call('optim', obj)

# two nls examples
n0 <- nls(y ~ beta1 - beta2 * exp(-beta3 * age), 
          data = vonB, 
          start = list(beta1 = 90, beta2 = 80, beta3 = .2))

# partial linearity (just pay attention to signs though)
n1 <- nls(y ~ cbind(1, exp(-beta3 * age)),
          data = vonB,
          start = list(beta3 = .2),
          algorithm = 'plinear')

