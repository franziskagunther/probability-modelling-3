# Modelling Probability Exercise 3

dat = read.csv(file = "ml-ex-3.csv")

neglogLikelihood <- function(theta, obs) {
  w_1 = theta[1]
  w_2 = theta[2]
  w_3 = theta[3]
  beta_1 = theta[4]
  beta_2 = theta[5]
  l = 10^7
  n = obs[1] 
  g = obs[1+c(1:n)] 
  y = obs[1+c((n+1):(2*n))] 
  wG = 1/(1+exp(-(beta_1 + beta_2*g)))
  logF = sum( (g==0)*log(w_1)+(g==1)*log(w_2)+(g==2)*log(w_3) + y*log(wG) + (1-y)*log(1-wG) )
  return(-sum(logF) + l*(w_1+w_2+w_3-1)^2) 
}

n <- length(dat$G)
obs <- c(n, dat$G, dat$Y)
theta_init <- c(c(0.1, 0.5, 0.4), c(1, 1)) 

out <- optim(theta_init, neglogLikelihood, gr = NULL, obs, method = "L-BFGS-B", lower = c(0.01, 0.01, 0.01, -100, -100), upper = c(0.99, 0.99, 0.99, 100, 100))

w_g_0 <- (1/(1+exp(-(out$par[4] + out$par[5]*0))))
w_g_1 <- (1/(1+exp(-(out$par[4] + out$par[5]*1))))
w_g_2 <- (1/(1+exp(-(out$par[4] + out$par[5]*2))))
 
cat(w_g_0, "w_g for socioeconomic status 0 \n")
cat(w_g_1, "w_g for socioeconomic status 1 \n")
cat(w_g_2, "w_g for socioeconomic status 2 \n")
