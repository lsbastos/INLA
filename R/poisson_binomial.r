# Exemplo de modelagem com duas verossimilhancas
# Y ~ Poisson(lambda_1)
# X|Y ~ Binomial(Y, theta)

library(INLA)
library(tidyverse)

N1 <- 100
N2 <- 100
lambda <- 15
theta <- 0.3

Y <- rpois(n = N1, lambda = lambda)
X <- rbinom(n = N2, size = Y, prob = theta)


# Univariate model 1
eq1 <- Y ~ 1
data.inla.1 <- tibble(Y = Y)

out1 <- inla(formula = eq1, 
             data = data.inla.1,
             family = "poisson", 
             control.compute = control.compute(waic = T))

exp(out1$summary.fixed)
out1$waic$waic


# Univariate model 2
eq2 <- Z ~ 1
data.inla.2 <- tibble(Z = X)
out2 <- inla(formula = eq2, 
             data = data.inla.2,
             family = "binomial",
             Ntrials = Y,
             control.compute = control.compute(waic = T))


arm::invlogit(out2$summary.fixed)
out2$waic$waic

# Join poisson-binomial model
eq.bi <- Z ~ -1 + I1 + I2
data.inla.bi <- list(
  Z = cbind(c(Y,rep(NA,N2)), 
            c(rep(NA,N1),X)),
  I1 = c(rep(1,N1),rep(NA,N2)),
  I2 = c(rep(NA,N1),rep(1,N2)),
  Ntrials = c(rep(NA,N1),Y))


out.bi <- inla(formula = eq.bi, 
               data = data.inla.bi,
               family = c("poisson","binomial"),
               Ntrials = data.inla.bi$Ntrials, 
               control.compute = control.compute(waic = T),
               control.predictor=list(
                 link=c(rep(1, N1),
                        rep(2, N2)),
                 compute = TRUE)
)


exp(out.bi$summary.fixed[1,])
arm::invlogit(out.bi$summary.fixed[2,])

out.bi$waic$waic
out.bi$summary.fitted.values %>% View()
