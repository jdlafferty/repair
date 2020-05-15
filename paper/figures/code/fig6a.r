library(ggplot2)
library(scales)

load("data/rf-50-3d.Rdata")

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
#group.colors = c("139"="black",  "185"="black",  "200"="black",  "267"="black",  "278"="black",  "312"="black",  "400"="black",  "417"="black",  "556"="black", "625"="black",  "741"="black",  "1111"="black", "1250"="black", "1667"="black", "2500"="black", "5000"="black", "6667"="black", "10000"="black")

#clr = hue_pal()(6)
clr = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")
group.colors = c("139"=clr[6],  "185"=clr[6],  "278"=clr[6],  "200"=clr[5],  "267"=clr[5],  "400"=clr[5],  "312"=clr[4], "417"=clr[4],  "625"=clr[4],  "556"=clr[3], "741"=clr[3],  "1111"=clr[3], "1250"=clr[2], "1667"=clr[2], "2500"=clr[2], "5000"=clr[1], "6667"=clr[1], "10000"=clr[1])


g = ggplot(dat, aes(epsilon, prob, linetype=p, colour=d)) + geom_point(shape=1, show.legend = FALSE) + geom_line(size=0.8) + xlab('epsilon') + ylab('probability of repair') + theme(legend.key.width=unit(1,"cm")) + guides(FALSE, colour=FALSE) + scale_color_manual(values=group.colors)
print(g)

pdf("../fig6a.pdf", width=6, height=4)
print(g)
dev.off()
