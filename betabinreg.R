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
  if(mu > 0 & mu < 1 & rho > 0 & rho < 1){
    rhofact <- (1-rho)/rho
    c(alpha=rhofact*mu,
      beta=rhofact*(1-mu))  
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
