#setwd('~/R/mlei_book_tmb')

library(TMB)
library(lme4)

# NB: Different data from the book?

compile('estrone.cpp')
dyn.load(dynlib('estrone'))


estrone <- read.table('http://users.du.se/~lrn/StatMod12/Lab4/estrone.dat',
                      header = TRUE)

data_list <- list(measurement = estrone$measurement,
                  obs_per_id = c(table(estrone$id)))

params <- list(intercept = 20,
               log_sigma = 2,
               log_sigma_re = 1.2,
               u = rep(0, length(unique(estrone$id))))

obj <- MakeADFun(data_list, params, random = c('u'),
                 DLL = 'estrone')

# test simulate
plot(obj$simulate()$measurement)

opt <- do.call('optim', obj)

obj_report <- sdreport(obj)
preds <- data.frame(value = obj_report$value,
                    std = obj_report$sd,
                    id = estrone$id,
                    t = rep(1:16, 5),
                    raw = estrone$measurement)

ggplot(preds, aes(x = t, y = value, colour = factor(id))) +
  geom_ribbon(aes(ymin = value - std, ymax = value + std), colour = 'gray70', alpha = .3) +
  geom_point()+
  facet_wrap(~id) +
  geom_point(colour = 'black', aes(y = raw))
  

ref_mod <- lmer(measurement ~ 1 + (1|id), data = estrone)