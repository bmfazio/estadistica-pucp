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
  c(alpha=((1/rho)-1)/(2-mu),
    betap=((1-mu)*(1/rho)-1)/(2-mu))
}

datagen <- function(x,coefs,lambda=3,rho=0.5,link=invlogit){
  
  n <- nrow(x)
  ntries <- rpois(n,lambda)+1
  
  mu <- link(as.matrix(x)%*%coefs)
  
  alpha <- (mu**2)*(((1-mu)/phi) - (1/mu))
  betap <- alpha*((1/mu)-1)
  
  cbind(data.frame(ntries,
                      nsucc=rbinom(n,ntries,rbeta(n,alpha,betap))),x)
  
}

bbt <- function(pars,obs,link=invlogit){

  n <- obs$ntries
  y <- obs$nsucc
  x <- as.matrix(obs[,-(1:2)])
  
  alpha <- pars[1]
  betap <- pars[2]
  
  # Calcular parametros de una beta canonica
  alpha <- (mu**2)*(((1-mu)/phi) - (1/mu))
  betap <- alpha*((1/mu)-1)
  
  # Log-verosimilitud de beta-binomial transformada
  -sum((sum(log(beta(y+alpha,n+betap-y)))-length(n)*log(beta(alpha,betap))))
  
}

###

plot(0:100,dbetabinom(0:100,100,0.5,200000000))
plot(0:100,dbinom(0:100,100,0.5))
