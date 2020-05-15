library(ggplot2)

load("data/ann-tanh-retrain=F-n=50-trials=100.Rdata")
p = c(156, 204, 278, 400, 626, 1110)
d = c(78, 102, 139, 200, 313, 555)

df = list()
ps = c()
K = 0
for (i in 1:length(p)) {
   cat(sprintf("p=%d\n", p[i]))
   W_error = colMeans(out2[[i]]$W_error)
   beta_error = colMeans(out2[[i]]$beta_error)
   K = K+1
   df[[K]] = data.frame(berror=beta_error, werror=W_error, epsilon=out2[[i]]$epsilon)
   ps = c(ps, p[i])
}

dat = c()
sz = c()
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

dat$p = rep(factor(ps[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, werror, color=p, linetype=p)) + geom_point(shape=1) + geom_line(size=0.8) + xlab('epsilon') + ylab('W error') + theme(legend.key.width=unit(1,"cm")) +  scale_linetype_manual(breaks=c("156","204", "278", "400", "626", "1110"), values=c(6,5,4,3,2,1))
print(g)

pdf("../fig7b.pdf", width=5, height=3.33)
print(g)
dev.off()

dat$p = rep(factor(ps[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, berror, color=p, linetype=p)) + geom_point(shape=1) + geom_line(size=0.8) + xlab('epsilon') + ylab('beta error') + theme(legend.key.width=unit(1,"cm")) +  scale_linetype_manual(breaks=c("156","204", "278", "400", "626", "1110"), values=c(6,5,4,3,2,1))
print(g)

  pdf("../fig7c.pdf", width=5, height=3.33)
print(g)
dev.off()
