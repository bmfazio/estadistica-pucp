## P4

# orings.csv

# a) A partir de los datos realice un análisis de regresión completo bajo inferencia bayesiana usando BUGS para explicar la variable falla en función de la temperatura teniendo en cuenta la selección de un MLG para una variable respuesta con distribución Bernoulli considerando los enlaces logit, power logit y power logit reciproco.

# Resuma los resultados del análisis en una tabla que compare el ajuste de los modelos considerando diversos indicadores. Presente una gráfica comparativa que muestra la probabilidad estimada en función de la temperatura.

###

library(coda)
library(mcmcplots)
library(R2WinBUGS)
bugs.dir<-"D:/bfazio/WinBUGS14"

# Datos

ori <- read.csv("D:/bfazio/maestria/lineales 2/Datos/orings.csv")
ori$temperatura <- scale(ori$temperatura)

X <- model.matrix(~ori$temperatura)
datos <- list(X=X,y=ori$falla,n=nrow(ori),p=ncol(X))
parametros <- c("beta")
iniciales <-function(){list(beta=rep(rnorm(1),ncol(X)))}

# Modelo logit
#   A priori no informativa

modelo <- function(){
  for (i in 1:n) {
    y[i] ~ dbern(mu[i])
    logit(mu[i]) <- eta[i]
    eta[i]<-inprod(beta[],X[i,])
  }
  for(j in 1:p){
    beta[j]~dnorm(0,0.000001)
  }
}

write.model(modelo, "m.bug")

sim.logit <- bugs(data = datos,inits = iniciales,
                  parameters.to.save = parametros,model.file="m.bug",
                  n.chains=2, n.iter=350000,n.burnin=50000,n.thin=30,
                  bugs.directory=bugs.dir,clearWD=TRUE, debug=FALSE)

# Diagnostico de convergencia
mcmcplot(sim.logit)

# Interpretacion
print(sim.logit,4)

##############################################################
#Diagnostico#

KL <- numeric()

for(i in 1:nrow(X)){
  eta=fit1$sims.matrix[,1:2]%*%X[i,]
  mu=exp(eta)/(1+exp(eta))
  f=dbinom(x = ori$falla[i],size = 1,prob = mu)
  lf=dbinom(x = ori$falla[i],size = 1,prob = mu,log =T)
  
  CPO=1/mean(1/f)
  
  KL[i]=mean(lf)-log(CPO)
}


plot(1:nrow(X), KL, type = "h", xlab = "Observación", ylab = "KL")
points(1:nrow(X),KL,pch=16,cex=1,col="dark red")
identify(KL,n=2)


# Prediccion (Temperatura = 66)
beta1=fit1$sims.matrix[,1]
beta2=fit1$sims.matrix[,2]

M=nrow(fit1$sims.matrix)

eta.new = beta1 + beta2*66
mu.new=exp(eta.new)/(1+exp(eta.new))
y.new=rbinom(n = M,size = 1,prob = mu.new)

summary(mu.new)
quantile(mu.new,probs=c(0.025,0.975))

summary(y.new)

###

#-------------------------------------------------------#
# Modelo Probit                                         #
#-------------------------------------------------------#
modelo <- function(){
  for (i in 1:n) {
    y[i] ~ dbern(mu[i])
    #mu[i] <- phi(eta[i])
    #probit(mu[i]) <- eta[i] 
    # truncado en  -xi, xi
    probit(mu[i]) <- eta[i] *(1-step( abs(eta[i])-xi )) - xi*step( -xi - eta[i] )+ xi *step( eta[i]-xi)
    eta[i]<-inprod(beta[],X[i,])
  }
  
  for(j in 1:p){
    beta[j]~dnorm(0,0.000001)
  }
  xi <- 8
}

write.model(modelo, "modelo2.bug")

iniciales <-function(){list(beta=rep(rnorm(1),ncol(X)) )}

start.time <- Sys.time()
fit2 <- bugs(data = datos,inits =  iniciales,
             parameters.to.save =  parametros,
             model.file="modelo2.bug",
             n.chains=2, n.iter=600000,
             n.burnin=200000,n.thin=5,
             bugs.directory=bugs.dir,
             clearWD=TRUE, debug=F)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


print(fit2,4)

#Diagnostico de Convergencia
plot(fit2)
mcmcplot(fit2)

###

# b) Realice un analisis comparativo del desempeño de los modelos en predecir de que ocurra una falla en un anillo de goma. Considerando un punto de corte de 0.5, presente la matriz de confusión para el modelo y luego presente una table comparativa considerando como criterios el error de clasificación, la sensibilidad, especificidad y AUC.

###

# c) Presente una gráfica comparativa que muestre las curvas ROC para los modelos considerados en (a).

###

# d) El día del accidente del transbordador había una temperatura de 31 °F. Considerando el mejor modelo encontrado en (b), realice una estimación puntual y por intervalo de la probabilidad de que ocurra una falla en un anillo de goma.

#######

# 4) niBBa

# a) 
library(gam)
library(ISLR)
library(MASS)

# Seleccion de obs
set.seed(281117)
train<-sort(sample(1:nrow(College),600))

# Estimacion de modelos

  # Seleccion de variables con modelo lineal
m1 <- glm(Outstate~.,family=gaussian,data=College[train,])
m1.s <- stepAIC(m1)

  # Selector de variables
fulltrans <- function(func,arg){
  as.formula(paste("Outstate~Private+",
                   paste0(paste0(func,"(",names(m1.s$coefficients)[-(1:2)],",",arg,")"),collapse="+")))}

m2 <- gam(fulltrans("s",1),family=gaussian,data=College[train,])
m2 <- gam(fulltrans("s",2),family=gaussian,data=College[train,])
m3 <- gam(fulltrans("s",4),family=gaussian,data=College[train,])
m4 <- gam(fulltrans("lo",0.25),family=gaussian,data=College[train,])
m5 <- gam(fulltrans("lo",0.75),family=gaussian,data=College[train,])

gam(Outstate ~ Private + lo(Apps, 0.25) + lo(Accept, 0.25) + lo(Enroll, 0.25) + lo(Top10perc, 0.25) + lo(Room.Board, 0.25) + lo(Personal, 0.25) + lo(Terminal,0.25) + lo(S.F.Ratio, 0.25) + lo(perc.alumni, 0.25) + lo(Expend, 0.25) + lo(Grad.Rate, 0.25),family=gaussian,data=College[train,])

Outstate ~ Private + lo(Apps, 0.25) + lo(Accept, 0.25) + lo(Enroll, 0.25) + lo(Top10perc, 0.25) + lo(Room.Board, 0.25) + lo(Personal, 0.25) + lo(Terminal,0.25) + lo(S.F.Ratio, 0.25) + lo(perc.alumni, 0.25) + lo(Expend, 0.25) + lo(Grad.Rate, 0.25)
