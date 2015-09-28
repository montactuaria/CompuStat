require(ggplot2)
require(plyr)

mc.intervals <- function(phi,N,sample=runif,alpha=0.05){
  results.list<-lapply(N,function(nsim){
    X<-sapply(FUN = X.dens,nsim)   #N samples de densidad X
    phiX<-sapply(X,phi)
    estim<-mean(phiX)
    S2<-var(phiX)
    quant<-qnorm(alpha/2,lower.tail = FALSE)
    int.upper<-estim + sqrt(S2/nsim)*quant
    int.lower<-estim - sqrt(S2/nsim)*quant
    return(data.frame(N=nsim,Estimate=estim,LI=int.lower,UI=int.upper))
  })
  results.table<-ldply(results.list)
  return(results.table)
}


phi<-function(x) 2*sqrt(4-x^2)
X.dens<- function(nsim) runif(nsim,0,2)
N<-seq(from=1000,to=10000,by=1000)
data<-mc.intervals(phi=phi,N=N,X.dens)


ggplot(data, aes(x=N)) +
  geom_ribbon(aes(ymin=LI, ymax=UI), fill="grey", alpha=.4) +
  geom_line(aes(y=Estimate), colour="blue") +
  geom_hline(aes(yintercept=pi) , colour="red", linetype="dotted", size=1)
