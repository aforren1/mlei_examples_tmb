
#setwd('~/R/mlei_book_tmb')
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

# test simulation
plot(obj$simulate()$y)

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

params2 <- list(beta1= opt$par[1], 
                beta2 = opt$par[2],
                beta3 = opt$par[3],
                log_sigma = opt$par[4])

obj2 <- MakeADFun(data_list, params2,
                  DLL = 'vonb')

sims <- replicate(1000, obj2$simulate())
tmp_sims <- sims
sims <- data.frame(simplify2array(sims))
sims$age <- data_list$age
sims <- reshape2::melt(sims, id.vars = c('age'))

ggplot(sims, aes(x = age, y = value, group = variable)) + 
  geom_line(colour = 'gray70', alpha = .3) + 
  geom_point(data = vonB, aes(x = age, y = y, group = NULL))

# try at parametric bootstrap equivalent
boottest <- lapply(FUN = function(x) {
  data_list$y <- tmp_sims[,x]
  objn <- MakeADFun(data_list, parameters = params2,
                    DLL = 'vonb')
  optn <- do.call('optim', objn)
  sdreport(objn)$value[2:17]
},
                   
  1:dim(tmp_sims)[2])

bootres <- data.frame(simplify2array(boottest))

bootres$age <- data_list$age
bootres <- reshape2::melt(bootres, id.vars = c('age'))

# very conservative (though is it, relative to sd?)
ggplot(bootres, aes(x = age, y = value, group = variable)) + 
  geom_line(colour = 'gray70', alpha = .3) + 
  geom_point(data = vonB, aes(x = age, y = y, group = NULL))