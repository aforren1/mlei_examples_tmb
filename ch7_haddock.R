#setwd('~/R/mlei_book_tmb')
library(TMB)
library(ggplot2)

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

ww <- sdreport(obj)
tmp <- data.frame(val = ww$value, std = ww$sd, x = data_list$x)

ggplot(tmp, aes(x = x, y = val)) + 
  geom_ribbon(aes(ymin = val - std, 
                  ymax = val + std), 
              colour = 'gray70', alpha = .3) + 
  geom_line() + 
  geom_point(aes(y = data_list$y/data_list$n))

# simulations with MLEs

params2 <- list(intercept = opt$par[1], 
                slope = opt$par[2])

obj2 <- MakeADFun(data_list, params2,
                 DLL = 'haddock')

sims <- replicate(1000, obj2$simulate()$y/data_list$n)

sim_df <- data.frame(obs = 1:37,
                     median = apply(sims, 1, function(x) median(x)),
                     lowci = apply(sims, 1, function(x) quantile(x, 0.025, na.rm = TRUE)),
                     hici = apply(sims, 1, function(x) quantile(x, 0.975, na.rm = TRUE)))

# discontinuity because there's a data point with zero observations
ggplot(sim_df, aes(x = obs, y = median)) + 
  geom_ribbon(aes(ymin = lowci, 
                  ymax = hici), 
              colour = 'gray70', alpha = .3) + 
  geom_line() + 
  geom_point(aes(y = data_list$y/data_list$n))