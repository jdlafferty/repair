library(ggplot2)

load("data/linear-50.Rdata")

df = list()
ps = c()
K = 0
for (i in 1:length(p)) {
   cat(sprintf("p=%d\n", p[i]))	
   prob = colSums(out[[i]]$success)/nrow(out[[i]]$success)
   K = K+1
   df[[K]] = data.frame(prob=prob, epsilon=out[[i]]$epsilon)
   ps = c(ps, p[i])
}

dat = c()
sz = c()
for (k in 4:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

dat$p = rep(factor(ps[4:K]),times=sz)
g = ggplot(dat, aes(epsilon, prob, color=p, linetype=p)) + geom_line(size=0.8) + xlab('epsilon') + ylab('probability of repair') + theme(legend.key.width=unit(1,"cm"))
print(g)

pdf("../fig1a.pdf", width=6, height=4)
print(g)
dev.off()

data = c()
sz = c()
alpha = .085
js = seq(9,1,-1)
for (k in 4:K) {
  df[[k]]$epsilon_adj = df[[k]]$epsilon + alpha*js[k] - .5
  data = rbind(data, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

data$p = rep(factor(ps[4:K]),times=sz)
g = ggplot(data, aes(epsilon_adj, prob, color=p, linetype=p)) + geom_line(size=0.8) + xlab('epsilon (adjusted)') + ylab('probability of repair') + xlim(0,1)  + theme(legend.key.width=unit(1,"cm"))
print(g)

pdf("../fig1b.pdf", width=6, height=4)
print(g)
dev.off()

