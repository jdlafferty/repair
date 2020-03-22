
library(quantreg)
library(mvtnorm)

run = function(n, p, trials=100, delta=.05) {

  eps = seq(0, 1, by=delta)
  success = matrix(rep(0,trials*length(eps)), trials, length(eps))

  for (t in 1:trials) {
    X = matrix(rnorm(n*p), n, p)
    y = rnorm(n)
    u = solve(X %*% t(X)) %*% y
    theta.hat = t(X) %*% u

    eta = theta.hat
    data = data.frame(t(X))

    for (i in 1:length(eps)) {
        corrupt = rbinom(p, 1, prob=eps[i])
        clean = rep(1,p) - corrupt
        Q = rnorm(p, mean=1)
        data$eta = clean*eta + corrupt*Q
        fit = rq(eta ~ 0 + ., data = data, tau=0.5, method='fn')
        u.hat = fit$coef
        theta.repaired = t(X) %*% u.hat
        norm = max(abs(theta.hat-theta.repaired))
	      if (norm < 1e-6) {
	        success[t,i] = 1
        }
        else {
          success[t,i] = 0
        }
    }
    if (t %% (trials/10) == 0){
      cat(sprintf('trial=%d\n', t))
    }
  }
  return(list(success=success, epsilon=eps))
}

plot.success = function(out, add=FALSE) {
   if (add) {
     lines(out$epsilon, colSums(out$success)/nrow(out$success))
   }
   else {
     plot(out$epsilon, colSums(out$success)/nrow(out$success), 'l', ylab='repair probability', xlab='epsilon')
   }
   points(out$epsilon, colSums(out$success)/nrow(out$success))
}


