library(ggplot2)
library(latex2exp)

load("data/linear-mean-shift-50.Rdata")
mu = round(mu, 3)

df = list()
mus = c()
K = 0
for (i in 1:length(out)) {
   cat(sprintf("i=%d\n", i))
   prob = colSums(out[[i]]$success)/nrow(out[[i]]$success)
   K = K+1
   df[[K]] = data.frame(prob=prob, epsilon=out[[i]]$epsilon)
   mus = c(mus, mu[i])
}

dat = c()
sz = c()
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

dat$mu = rep(factor(mus[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, prob, color=mu, linetype=mu)) + geom_line(size=0.8) + xlab('epsilon') + ylab('probability of repair') + theme(legend.key.width=unit(1,"cm")) + scale_linetype_discrete(name = TeX('mean $\\mu$')) + scale_color_discrete(name = TeX('mean $\\mu$'))
print(g)

pdf("../fig5.pdf", width=6, height=4)
print(g)
dev.off()
