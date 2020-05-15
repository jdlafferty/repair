library(ggplot2)

load("data/linear-sum-50.Rdata")

df = list()
ps = c()
K = 0
for (i in 1:length(p)) {
   cat(sprintf("p=%d\n", p[i]))
   prob = 1-colSums(out[[i]]$success/p[i])/nrow(out[[i]]$success)
   wrob = colSums(out[[i]]$success==0)/nrow(out[[i]]$success)
   K = K+1
   df[[K]] = data.frame(prob=prob, wrob=wrob, epsilon=out[[i]]$epsilon)
   ps = c(ps, p[i])
}

dat = c()
sz = c()
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

dat$p = rep(factor(ps[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, prob, color=p, linetype=p)) + geom_line(size=0.8) + xlab('epsilon') + ylab('coefficient probability of repair') + theme(legend.key.width=unit(1,"cm"))
print(g)

pdf("../fig4a.pdf", width=6, height=4)
print(g)
dev.off()

g = ggplot(dat, aes(epsilon, wrob, color=p, linetype=p)) + geom_line(size=0.8) + xlab('epsilon') + ylab('probability of repair') + theme(legend.key.width=unit(1,"cm"))
print(g)

pdf("../fig4b.pdf", width=6, height=4)
print(g)
dev.off()
