invlogit <- function(x)exp(x)/(1+exp(x))

xgen <- function(n=1000,nvar=3){
  if(nvar > 0){
    xdf<-data.frame(lapply(1:nvar,function(x)rnorm(n,sd=0.5)))
    names(xdf) <- paste0("var",1:nvar)
    
    return(cbind(const=1,xdf) )
  } else{
    matrix(1,nrow=n)
  }
}

murho2ab <- function(mu,rho){
  if(all(mu > 0 & mu < 1 & rho > 0 & rho < 1)){
    rhofact <- (1-rho)/rho
    list(alpha=rhofact*mu,
      betap=rhofact*(1-mu))  
  } else{
    warning("mu, rho must be between 0 and 1")
  }
}

datagen <- function(x,coefs,lambda=3,rho=0.5,link=invlogit){
  
  n <- nrow(x)
  ntries <- rpois(n,lambda)+1
  
  mu <- link(as.matrix(x)%*%coefs)
  
  alpha <- murho2ab(mu,rho)[["alpha"]]
  betap <- murho2ab(mu,rho)[["betap"]]
  
  cbind(data.frame(ntries,
                   nsucc=rbinom(n,ntries,rbeta(n,alpha,betap))),x)
}

bbt <- function(pars,obs,link=invlogit){

  n <- obs$ntries
  y <- obs$nsucc
  x <- as.matrix(obs[,-(1:2)])
  
  coefs <- pars[-1]
  mu <- link(as.matrix(x)%*%coefs)
  rho <- pars[1]
  
  alpha <- murho2ab(mu,rho)[["alpha"]]
  betap <- murho2ab(mu,rho)[["betap"]]
  
  # Log-verosimilitud
  -sum((sum(log(beta(y+alpha,n+betap-y)))-length(n)*log(beta(alpha,betap))))
}

### Simulacion
modbetas <- c(0.2,-0.4,0.1,0.25)
simures <- matrix(nrow=1000,ncol=5)

for(i in 1:nrow(simures)){
  simdata <- datagen(xgen(),coefs=modbetas,rho = 0.5)
  simures[i,] <- optim(par = c(0.1,0.5,0.5,0.5,0.5),bbt,obs=simdata)$par
  print(i)
}

par(mfrow=c(1,5))
plot(density(simures[,1]))
abline(v=0.5,col="red")
plot(density(simures[,2]))
abline(v=modbetas[1],col="red")
plot(density(simures[,3]))
abline(v=modbetas[2],col="red")
plot(density(simures[,4]))
abline(v=modbetas[3],col="red")
plot(density(simures[,5]))
abline(v=modbetas[4],col="red")
