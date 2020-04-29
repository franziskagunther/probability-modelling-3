# Modelling Probability Exercise 3

dat = read.csv(file = "ml-ex-3.csv")

neglogLikelihood <- function(theta, obs) {
  w = theta[1:3]
  beta_1 = theta[4]
  beta_2 = theta[5]
  n = obs[1] 
  g = obs[1+c(1:n)] 
  y = obs[1+c((n+1):(2*n))] 
  x = c(55, 425, 519) 
  x_multi = c(163, 712, 625) 
  logF = dmultinom(x=c(x_multi[1], x_multi[2], x_multi[3]), size=1500, prob=c(w[1], w[2], w[3]), log=TRUE) * dbinom(x=x[g+1], size=x_multi[g+1], prob=(1/(1+exp(-(beta_1 + beta_2*g)))), log=TRUE)
  return(-sum(logF)) 
}

n <- length(dat$G)
obs <- c(n, dat$G, dat$Y)
theta_init = c(c(0.1, 0.5, 0.4), c(0, 0)) 

out <- optim(theta_init, neglogLikelihood, gr = NULL, obs, method = "L-BFGS-B", lower = c(0.01, 0.01, 0.01, -1000, -1000), upper = c(0.99, 0.99, 0.99, 1000, 1000))

