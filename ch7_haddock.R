library(TMB)

compile('haddock.cpp')
dyn.load(dynlib('haddock'))


haddock <- read.table(paste0('https://www.stat.auckland.ac.nz/', 
                       '~millar/MLEI/Data/Clark113_20min.dat'), 
                      header = TRUE)

data_list <- list(y = haddock$codend, 
                  n = haddock$codend + haddock$cover,
                  x = haddock$lenclass)

params <- list(intercept = -5,
               slope = .3)

obj <- MakeADFun(data_list, params,
                 DLL = 'haddock')

# simulate example
plot(obj$simulate()$y/data_list$n)

low <- list(p = 0)
high <- list(p = 1)

opt <- do.call('optim', obj)

print(summary(sdreport(obj)))

comp_glm <-glm(codend/I(codend + cover) ~ lenclass, 
               data = haddock, family = 'binomial', 
               weights = codend + cover)

print(summary(comp_glm)$coefficients)