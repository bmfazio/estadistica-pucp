# Logistica inversa
invlogit <- function(x)exp(x)/(1+exp(x))

# Generar una matriz de X aleatorios
# Por el momento, son iid N(0,0.5)
xgen <- function(n=1000,nvar=3){
  if(nvar > 0){
    xdf<-data.frame(lapply(1:nvar,function(x)rnorm(n,sd=0.5)))
    names(xdf) <- paste0("var",1:nvar)
    
    return(cbind(const=1,xdf) )
  } else{
    matrix(1,nrow=n)
  }
}

# Transformar mu y rho al alfa y beta que pide R para la beta
murho2ab <- function(mu,rho){
  if(all(mu > 0 & mu < 1 & rho > 0 & rho < 1)){
    rhofact <- (1-rho)/rho
    list(alpha=rhofact*mu,
         betap=rhofact*(1-mu))  
  } else{
    warning("mu, rho must be between 0 and 1")
  }
}

# Dado un vector de coeficientes y matriz de disenho,
# generar las observaciones correspondientes
datagen <- function(x,coefs,lambda=3,rho=0.5,link=invlogit){
  
  n <- nrow(x)
  ntries <- rpois(n,lambda)+1
  
  mu <- link(as.matrix(x)%*%coefs)
  
  alpha <- murho2ab(mu,rho)[["alpha"]]
  betap <- murho2ab(mu,rho)[["betap"]]
  
  cbind(data.frame(ntries,
                   nsucc=rbinom(n,ntries,rbeta(n,alpha,betap))),x)
}

# LL betabinomial
bbt <- function(pars,obs,link=invlogit,vgam=FALSE){
  
  n <- obs$ntries
  y <- obs$nsucc
  x <- as.matrix(obs[,-(1:2)])
  
  coefs <- pars[-1]
  mu <- link(as.matrix(x)%*%coefs)
  rho <- pars[1]
  
  alpha <- murho2ab(mu,rho)[["alpha"]]
  betap <- murho2ab(mu,rho)[["betap"]]
  
  # Log-verosimilitud con signo negativo
  simmanu <- -sum((sum(log(beta(y+alpha,n+betap-y)))-length(n)*log(beta(alpha,betap))))
  return(simmanu)
}

### Data real
library(rio)
library(data.table)
msm <- import("C:/Users/ASUS/Desktop/Hughes-Bayes/Datasets/msm_risk_new5.dta", setclass = "data.table")

rbind(msm[,.(ins=qsmp1isx,rec=qsmp1rsx)],
      msm[,.(ins=qsmp2isx,rec=qsmp2rsx)],
      msm[,.(ins=qsmp3isx,rec=qsmp3rsx)]) -> sxdat

sxdat[!is.na(ins)&!is.na(rec),.(ntries=ins+rec,nsucc=ins)]
sxdat[,const:=1]
#sxdat[,ntries:=as.numeric(ntries)]

okz <- optim(par = c(0.5,0.5),bbt,obs=sxdat)$par
betapars <- murho2ab(invlogit(okz[2]),okz[1])

curve(dbeta(x,betapars[[1]],betapars[[2]]))






### Simulacion

# Contenedor para los resultados de simulacion
# (numero de filas = numero de simulaciones)
simuresManu <- matrix(nrow=1000,ncol=5)

# Vector de coeficientes
modbetas <- c(0.2,-0.4,0.1,0.25)

set.seed(1)
for(i in 1:nrow(simuresManu)){
  simdata <- datagen(xgen(),coefs=modbetas,rho = 0.5)
  simuresManu[i,] <- optim(par = c(0.1,0.5,0.5,0.5,0.5),bbt,obs=simdata)$par
  print(i)
}
set.seed(1)

par(mfrow=c(1,2))
plot(density(simuresManu[,2]),main = "beta0")
abline(v=modbetas[1],col="red")
plot(density(simures[,3]),main = "beta1")
abline(v=modbetas[2],col="red")
plot(density(simures[,4]),main = "beta2")
abline(v=modbetas[3],col="red")
plot(density(simures[,5]),main = "beta3")
abline(v=modbetas[4],col="red")
plot(density(simures[,1]),main = "phi")
abline(v=0.5,col="red")
