library("futbol")
library("rjags")
library("plyr")

d <- laliga

categorical = "
model{

for (s in 1:nS){
  pi[,s] ~ ddirich(a)
}

#pi ~ ddirich(a)

for(i in 1:n){
  y[i] ~ dcat(pi[,seas[i]])
}

}
"

dat <- list(n=nrow(d), y=as.numeric(as.factor(d$FinalRes)), a=rep(1,3),
            seas = as.numeric(as.factor(d$Season)), nS = plyr:::nunique(d$Season))
m = jags.model(textConnection(categorical), data=dat, n.chains = 1, n.adapt=1000, quiet=FALSE)
res = coda.samples(m, c("pi"), n.iter=2000, thin=10, burn.in=niter/10)

summary(res)

plot(res[,"pi[1]"])
plot(res[,"pi[2]"])
plot(res[,"pi[3]"])


