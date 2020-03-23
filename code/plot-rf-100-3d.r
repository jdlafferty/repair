library(ggplot2)

load("rf-100-3d.Rdata")

df = list()
ps = c()
ds = c()
K = 0
for (i in 1:length(p)) {
   cat(sprintf("p=%d\n", p[i]))	
   prob = colSums(out[[i]]$success)/nrow(out[[i]]$success)
   K = K+1
   df[[K]] = data.frame(prob=prob, epsilon=out[[i]]$epsilon)
   ps = c(ps, p[i])
   ds = c(ds, d[i])
}

dat = c()
sz = c()
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

dat$p = rep(factor(ps),times=sz)
dat$d = rep(factor(ds),times=sz)
group.colors = c("278"="black",  "371"="black",  "556"="black",  "400"="black",  "533"="black",  "800"="black",  "625"="black",  "833"="black",  "1250"="black", "1111"="black",  "1481"="black",  "2222"="black", "2500"="black", "3333"="black", "5000"="black", "10000"="black", "13333"="black", "20000"="black")
g = ggplot(dat, aes(epsilon, prob, linetype=p, colour=d)) + geom_point(shape=1, show.legend = FALSE) + geom_line(size=0.8) + xlab('epsilon') + ylab('probability of repair') + theme(legend.key.width=unit(1,"cm")) + guides(FALSE, colour = FALSE) + scale_color_manual(values=group.colors)
print(g)

pdf("plot-rf-100.pdf", width=6, height=4)
print(g)
dev.off()
