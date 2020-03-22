

source("repair2new.r")

out = list()
n = 100
p = round(n*200/seq(9,1,by=-1)^2)

for (i in 1:length(p)) {
  cat(sprintf("n=%d, p=%d\n", n, p[i]))
  out[[i]] = run(n, p[i], trials=200, delta=.025)
  plot.success(out[[i]], add=(i>1))
}

for (i in 1:length(out)) { plot.success(out[[i]], add=(i>1))}

pdf('repair-linear-100.pdf')
for (i in 1:length(out)) { plot.success(out[[i]], add=(i>1))}
dev.off()

save(n, p, out, file="linear-100.Rdata")
