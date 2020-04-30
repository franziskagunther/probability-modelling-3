# Modelling Probability Exercise 3

dat = read.csv(file = "ml-ex-3.csv")

neglogLikelihood <- function(theta, obs) {
  w_1 = theta[1]
  w_2 = theta[2]
  w_3 = theta[3]
  beta_1 = theta[4]
  beta_2 = theta[5]
  n = obs[1] 
  g = obs[1+c(1:n)] 
  y = obs[1+c((n+1):(2*n))] 
  x = c(55, 425, 519) 
  g_0 = length(g[g == 0])
  g_1 = length(g[g == 1])
  g_2 = length(g[g == 2])
  y_1 = c(length(y[g == 0][y[g == 0] == 1]), length(y[g == 1][y[g == 1] == 1]), length(y[g == 2][y[g == 2] == 1]))
  logF = dmultinom(x=c(g_0, g_1, g_2), size=1500, prob=c(w_1, w_2, w_3), log=TRUE) + dbinom(x=y_1[g+1], size=c(g_0, g_1, g_2)[g+1], prob=(1/(1+exp(-(beta_1 + beta_2*g)))), log=TRUE)  
  return(-sum(logF)) 
}

n <- length(dat$G)
obs <- c(n, dat$G, dat$Y)
theta_init <- c(c(0.1, 0.5, 0.4), c(1, 1)) 

out <- optim(theta_init, neglogLikelihood, gr = NULL, obs, method = "L-BFGS-B", lower = c(0.01, 0.01, 0.01, -100, -100), upper = c(0.99, 0.99, 0.99, 100, 100))

w_g_0 <- (1/(1+exp(-(out$par[4] + out$par[5]*0))))
w_g_1 <- (1/(1+exp(-(out$par[4] + out$par[5]*1))))
w_g_2 <- (1/(1+exp(-(out$par[4] + out$par[5]*2))))

cat(w_g_0, "w_g for socioeconomic status 0")
cat(w_g_1, "w_g for socioeconomic status 1")
cat(w_g_2, "w_g for socioeconomic status 2")
