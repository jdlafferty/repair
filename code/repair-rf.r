
library(quantreg)
library(mvtnorm)

gradient.descent = function(y, X, step=1, verbose=TRUE) {
  n = length(y)
  p = dim(X)[2]
  beta = rep(0, p)
  T = 1000
  C = step/n
  thresh = 1e-6/n
  while(TRUE) {
    for (t in 1:T) {
      R = y - X %*% beta
      beta = beta + C/t * t(X) %*% R
      loss = sum(R^2)/(2*n)
      if ((loss < thresh) & (verbose)) {
        cat(sprintf("Converged (loss=%.2e) in %d steps\n", loss, t))
        break
      }
    }
    if (t>=T) {
      cat(sprintf("Max iterations T=%d reached; loss=%.2e\n", T, loss))
      step = 2*step
      C = step/n
      beta = rep(0, p)
      cat(sprintf("Doubling step size to %.2f and restarting\n ", step))
    }
    else if (max(beta) > 1000) {
      cat(sprintf("Exploding parameters: %f\n", max(beta)))
      step = step/2
      C = step/n
      beta = rep(0, p)
      cat(sprintf("Halving step size to %.2f and restarting\n ", step))
    }
    else {
       break
    }
  }
  return(list(beta=beta, loss=loss, step=step))
}

run = function(n, d, p, trials=1, step=1, delta=.05) {

  eps = seq(0, 1, by=delta)
  success = matrix(rep(0,trials*length(eps)), trials, length(eps))

  for (t in 1:trials) {
    Z = matrix(rnorm(n*d), n, d)
    W = matrix(rnorm(p*d, sd=sqrt(1./d)), p, d)
    X = t(tanh(W %*% t(Z)))
    beta.star = rnorm(p)
    y = X %*% beta.star

    gd = gradient.descent(y, X, step=step)
    #step = gd$step
    beta.hat = gd$beta
    eta = beta.hat
    data = data.frame(t(X))

    for (i in 1:length(eps)) {
        corrupt = rbinom(p, 1, prob=eps[i])
        clean = rep(1,p) - corrupt
        Q = rnorm(p, mean=1)
        data$eta = clean*eta + corrupt*Q
        fit = rq(eta ~ 0 + ., data = data, tau=0.5, method='fn')
        u.hat = fit$coef
        beta.repaired = t(X) %*% u.hat
        norm = max(abs(beta.hat-beta.repaired))
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
