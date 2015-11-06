library(Rcpp)
library(dplyr)

#Tarea predecir Sepal.Length con las otras variables


# ESTIMATION OF A MEAN
data(iris)

Y <- iris$Sepal.Length
N <- dim(iris)[1]
X <- as.matrix(cbind(1,iris[,2:4]))
beta.hat<-solve(t(X)%*%X,t(X)%*%Y)
pred<-X%*%beta.hat
residuals<-Y-pred
#hist(residuals,breaks = 20)
SS<-sqrt(sum((Y-pred)^2)/(N-dim(iris)[2]+1))
cov.betas<-(SS^2)*solve(t(X)%*%X)
sqrt(diag(cov.betas))


# APPROACH 1) POINTWISE ESTIMATORS AND PIVOTAL QUANTITIES

lm <- lm(Sepal.Length ~ ., iris)
summary(lm)

# BAYESIAN APPROACH

# beta_j ~ N(0,10)
# Se puede jugar con los parámetros de la inicial aquí y en 1). Cuando es muy plana los coeficientes se parecen mucho a los de la regresión lineal
prior.beta <- function(x) dnorm(x, 0, .2)
prior.sd <- function(x) dgamma(x,5,100)
plot(prior.beta, col="darkblue", xlim=c(-50,50), lwd="2", main="Prior for mean", ylab="density")
plot(prior.sd, col="darkblue", xlim=c(-0,1), lwd="2", main="Prior for mean", ylab="density")


# 1) logposterior distribution (up to a constant)
cppFunction('
            double objdens(NumericMatrix X, NumericVector y, NumericVector theta){
            int i;
            double lkh, logprior, yhat;
            int m=X.nrow(), p=X.ncol();
            NumericVector beta(m-1);
            double sd;
            for (i=0; i<p; i++){
              beta[i] = theta[i];
            }
            sd = theta[p]; 
            NumericVector aux(m);
            // Compute loglikelihood
            lkh=0;
            for (int i=0; i<m; i++){
            aux = X(i,_)*beta;
            yhat = std::accumulate(aux.begin(), aux.end(), 0.0);
            lkh += -.5*pow((y[i] - yhat)/sd,2) - log(sd);
            }
            // Compute logprior
            logprior = 0.0;
            for(int j=0; j<p; j++){
            logprior += R::dnorm(beta[j], 0.0, 0.8, true); // Aquí la inicial!!
            }
            logprior += R::dgamma(sd, 5.0, 0.1,  true);
            // Log of target density
            return lkh + logprior;
            }')


# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
            NumericVector proposal(NumericVector theta, NumericMatrix X){
            int nparam = theta.size();
            int m=X.nrow();
            double jump = 0.25/sqrt(m);   //checar paper 
            NumericVector newtheta(nparam);
            for (int i=0; i<nparam; i++){
            newtheta[i] = R::rnorm(theta[i], jump);
            }
            return newtheta;
            }')



# 3) METROPOLIS

sourceCpp("MHBayesLinReg.cpp")

nsim <- 10000
init <- c(0,0,0,0.5)

proposal(init,X)

mh.samp <- MHBayesLinReg(nsim, init, objdens, proposal, X, Y)
estims <- mh.samp$theta



#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,nrow(estims),by=5)
plot(estims[pts, ], type="l", asp=1)
###aceptacion
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
acc.rate<- 1-cumsum(rejections)/cumsum(trials)


plot(100*acc.rate,type = 'l',ylim = c(0,100), main = "Acceptance rate", ylab = "%")

### 2) AUTOCORRELATION
par(mfrow=c(2,3))
for(i in 1:ncol(estims)){
  acf(estims[ , i],main=paste("theta",i))
}


# burnin and subsampling
burnin <- round(0.2*sim)
estims <- estims[-(1:burnin), ]
