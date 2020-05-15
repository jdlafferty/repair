library(ggplot2)

load("data/ann-tanh-retrain-n=50-trials=100.Rdata")

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
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

dat$p = rep(factor(ps[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, prob, color=p, linetype=p)) + geom_point(shape=1) + geom_line(size=0.8) + xlab('epsilon') + ylab('probability of repair') + theme(legend.key.width=unit(1,"cm")) +  scale_linetype_manual(breaks=c("156","204", "278", "400", "626", "1110"), values=c(6,5,4,3,2,1))
print(g)

pdf("../fig7a.pdf", width=5, height=3.33)
print(g)
dev.off()
