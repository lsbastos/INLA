# Exemplo de modelagem com duas verossimilhancas
# Y_1 ~ Poisson(lambda_1)
# Y_2 ~ Poisson(lambda_2)

library(INLA)
library(tidyverse)

N1 <- 100
N2 <- 100
lambda <- c(2, 4)

Y1 <- rpois(n = N1, lambda = lambda[1])
Y2 <- rpois(n = N2, lambda = lambda[2])


# Univariate model 1
eq1 <- Y ~ 1
data.inla.1 <- tibble(Y = Y1)

out1 <- inla(formula = eq1, 
             data = data.inla.1,
             family = "poisson", 
             control.compute = control.compute(waic = T))

exp(out1$summary.fixed)
out1$waic$waic


# Univariate model 2
eq2 <- Y ~ 1
data.inla.2 <- tibble(Y = Y2)
out2 <- inla(formula = eq2, 
             data = data.inla.2,
             family = "poisson",
             control.compute = control.compute(waic = T))


exp(out2$summary.fixed)
out2$waic$waic

# Forecasting (nao precisa prever o mesmo numero de casos)
K1 = 15
K2 = 10

# Bivariate model
eq.bi <- Y ~ -1 + I1 + I2
data.inla.bi <- list(
  Y = cbind(c(Y1,rep(NA,N2), rep(NA,K1+K2)), 
            c(rep(NA,N1),Y2, rep(NA,K1+K2))),
  I1 = c(rep(1,N1),rep(NA,N2), rep(1,K1), rep(NA,K2)),
  I2 = c(rep(NA,N1),rep(1,N2), rep(NA,K1), rep(1,K2))
  )


out.bi <- inla(formula = eq.bi, 
             data = data.inla.bi,
             family = c("poisson","poisson"),
             control.compute = control.compute(waic = T),
             control.predictor=list(
               link=c(rep(1, N1),
                      rep(2, N2),
                      rep(1, K1),
                      rep(2, K2)),
               compute = TRUE)
             )


exp(out.bi$summary.fixed)
out.bi$waic$waic
out.bi$summary.fitted.values %>% View()
