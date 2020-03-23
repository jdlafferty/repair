

source("repair-rf.r")

out = list()
n = 100
p = round(n*200/seq(6,1,by=-1)^2)
d = round(as.vector(rbind(p/2,p*2/3,p)))
p = as.vector(rbind(p,p,p))
step = c(5, 4, 2, 1, .5, .1)
step = as.vector(rbind(step, step, step))

for (i in 1:length(p)) {
  cat(sprintf("\n\nn=%d, d=%d p=%d step=%.2f\n=============================\n", n, d[i], p[i], step[i]))
  out[[i]] = run(n, d[i], p[i], trials=200, step=step[i], delta=.025)
  plot.success(out[[i]], add=(i>1))
}

pdf('repair-rf-n100-3d.pdf')
for (i in 1:length(out)) { plot.success(out[[i]], add=(i>1))}
dev.off()

save(n, d, p, out, file="rf-100-3d.Rdata")
