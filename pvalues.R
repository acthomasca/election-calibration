#################################################################################
#
# p-value test of 2012 Presidential Election Predictions.
# A.C. Thomas, Nov 7, 2012
#
#################################################################################


res.table <- read.csv("results-with-error.csv")


res.table$voteshare <- with(res.table, demvotetrue/(demvotetrue+repvotetrue))
res.table$twopartydem538 <- with(res.table, demshare538/(demshare538+repshare538))
res.table$demsharelinzer <- res.table$demvotelinzer/100




plot(res.table$twopartydem538, res.table$voteshare)
plot(res.table$demsharelinzer, res.table$voteshare)
plot(res.table$demsharelinzer, res.table$twopartydem538)

pvalue.538 <- with(res.table, pnorm(2*(twopartydem538-voteshare)*100/twosigma538))
plot(pvalue.538, main="538")
axis(1, 1:50, res.table$X)

pvalue.linzer <- with(res.table, pnorm(2*(demsharelinzer-voteshare)*100/twosigmalinzer))
points(pvalue.linzer, col=2)
plot(pvalue.linzer, main="Linzer")
axis(1, 1:50, res.table$X)

plot(pvalue.538, pvalue.linzer, ty="n")
text(pvalue.538, pvalue.linzer, as.character(res.table[,1]))

hist(pvalue.538)
hist(pvalue.linzer)

png("uniform-plots.png", height=400, width=800)
par(mfrow=c(1,2))
plot(seq(0,1,length=50), sort(pvalue.538), main="538 p-values"); abline(a=0,b=1,col=2)
plot(seq(0,1,length=50), sort(pvalue.linzer), main="Votamatic p-values"); abline(a=0,b=1,col=2)
dev.off()

png("538-uniform-plots.png", height=400, width=400)
plot(seq(0,1,length=50), sort(pvalue.538), main="538 p-values", xlab="Sorted order", ylab="Sorted p-values"); abline(a=0,b=1,col=2)
dev.off()

png("votamatic-uniform-plots.png", height=400, width=400)
plot(seq(0,1,length=50), sort(pvalue.linzer), main="Votamatic p-values", xlab="Sorted order", ylab="Sorted p-values"); abline(a=0,b=1,col=2)
dev.off()



richards <- read.csv("richards-table_voteprop.dat")
twosigrichards <- (richards[,4]-richards[,3])/2

pvalue.richards <- with(res.table, pnorm(2*(richards[,2]-voteshare)/twosigrichards))

plot(pvalue.richards)
axis(1, 1:50, res.table$X)
x11(); plot(seq(0,1,length=50), sort(pvalue.richards), main="Richards p-values", xlab="Sorted order", ylab="Sorted p-values"); abline(a=0,b=1,col=2)



jackman.table <- read.csv("evOutExtra-jackman-2.csv")
pvalue.jackman <- with(res.table, pnorm((jackman.table$Obama/100-voteshare)*100/jackman.table$sd))
plot(pvalue.jackman)
axis(1, 1:50, res.table$X)

pdf("jackman-pvalues.pdf")
plot(seq(0,1,length=45), sort(pvalue.jackman[!is.na(pvalue.jackman)]), main="HuffPost p-values", xlab="Sorted order", ylab="Sorted p-values"); abline(a=0,b=1,col=2)
dev.off()
rmse.jackman <- sqrt(mean(((1:45/46)-sort(pvalue.jackman[!is.na(pvalue.jackman)]))^2))

rmse.538 <- sqrt(mean(((1:50/51)-sort(pvalue.538))^2))
rmse.linzer <- sqrt(mean(((1:50/51)-sort(pvalue.linzer))^2))


plot(pvalue.538, pvalue.jackman, ty="n")
text(pvalue.538, pvalue.jackman, as.character(res.table[,1]))
