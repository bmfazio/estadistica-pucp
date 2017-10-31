setwd("D:/Clases/estadisticaPUCP/Lineales 2/")
require(MASS)

# Descripcion de comandos diag._
# Una salida con los siguientes graficos:
# 1) Influencia en Localizacion. Estimados vs Distancia Cook. Linea es 2 veces DCook media.
# 2) Influencia en Localizacion/Escala. DCook ajustada que da influencia en las dos medidas.
# 3) Influencia Local. Dmax mide variacion mas grande causada por pequenas perturbaciones.
# 4) Leverage. Indica puntos que tienen el potencial de influir en los estimados. Sirve para detectar extrapolacion.
# 5) Outliers. Detecta residuos demasiado grandes.
# 6) Funcion de varianza. Residuos absolutos permiten evaluar si existe una tendencia en la varianza.

# El comando envel._ da los outliers conjuntos.

####################################################################
# Modelo Gamma                                                     #
####################################################################

#Distribución gamma para MU = 1 fijo
par(mfrow=c(2,3))
curve(dgamma(x, rate=0.5, shape=0.5),from=0, to=5)
curve(dgamma(x, rate=1, shape=1),from=0, to=5)
curve(dgamma(x, rate=2, shape=2),from=0, to=5)
curve(dgamma(x, rate=4, shape=4),from=0, to=5)
curve(dgamma(x, rate=6, shape=6),from=0, to=5)
curve(dgamma(x, rate=8, shape=8),from=0, to=5)
par(mfrow=c(1,1))

####################################################################
# # Ejemplo 1: Turbina                                           # #
####################################################################

turbina = scan("turbina.dat", list(tipo=0, duracion=0))
attach(turbina)
tipo = factor(tipo)

# Análisis descriptivo
plot(density(duracion))
boxplot(split(duracion, tipo), ylab="Tiempo de Duración")
turbina.m<-tapply(X=duracion,INDEX=tipo,FUN=mean)
turbina.m
turbina.sd<-tapply(X=duracion,INDEX=tipo,FUN=sd)
turbina.sd
turbina.cv<-turbina.sd/turbina.m*100
turbina.cv

# Estimación usando el modelo lineal normal
ajuste4.turbina = lm(duracion ~ tipo)
#source.with.encoding('envel.norm.R', encoding='ISO-8859-1')
#source('envel.norm.txt')
source('http://www.poleto.com/funcoes/envel.norm.txt')
envel.norm(ajuste4.turbina,iden=2)

#source.with.encoding('diag.norm.R', encoding='ISO-8859-1')
#source('diag.norm.txt')
source('http://www.poleto.com/funcoes/diag.norm.txt')
diag.norm(ajuste4.turbina,iden=rep(1,6))

# Estimación usando el modelo Gamma
# Ignorando las turbinas
ajuste1.turbina = glm(duracion ~1, family=Gamma)
summary(ajuste1.turbina)
gamma.shape(ajuste1.turbina) #La estimación confirma la asimetría hacia la derecha
curve(dgamma(x, rate=4.0155211/9.9766, shape=4.0155211),from=0, to=30)

# Efecto de Turbinas
ajuste2.turbina = glm(duracion ~ tipo, family=Gamma(link=identity))
summary(ajuste2.turbina)
gamma.shape(ajuste2.turbina)
# Desvío del Modelo
Dstar<-ajuste2.turbina$deviance*gamma.shape(ajuste2.turbina)$alpha
Dstar
1-pchisq(q = Dstar,df =  ajuste2.turbina$df.residual)

# Inferencia
# Agrupando los tipos I, III y IV
# H0: B4 = B3 = 0
tipo1 = rep(c(1,2,1,1,5), 10)
tipo1 = factor(tipo1)
ajuste3.turbina = glm(duracion ~ tipo1, family=Gamma(link=identity))
summary(ajuste3.turbina)
gamma.shape(ajuste3.turbina)

# Prueba F
D0<-ajuste3.turbina$deviance
D1<-ajuste2.turbina$deviance
gl0<-ajuste3.turbina$df.residual
gl1<-ajuste2.turbina$df.residual
Fc<-((D0-D1)/(gl0-gl1))/(D1/gl1)
Fc
1-pf(q = Fc,df1 = (gl0-gl1),df2 = gl1)
anova(ajuste3.turbina,ajuste2.turbina,test = "F")

# Desvío del Modelo
Dstar<-ajuste3.turbina$deviance*gamma.shape(ajuste3.turbina)$alpha
Dstar
1-pchisq(q = Dstar,df =  ajuste3.turbina$df.residual)

# Diagnóstico
#source.with.encoding('envel.gama.R', encoding='ISO-8859-1')
#source('envel.gama.txt')
source('http://www.poleto.com/funcoes/envel.gama.txt')
envel.gama(ajuste3.turbina)

#source.with.encoding('diag.gama.R', encoding='ISO-8859-1')
#source('diag.gama.txt')
source('http://www.poleto.com/funcoes/diag.gama.txt')
diag.gama(ajuste3.turbina)
diag.gama(ajuste3.turbina,iden=c(2,0,2,0,0,0,0,0))

# Efecto de posibles observaciones influenciales
out=c(49)
res1=round(summary(ajuste3.turbina)$coef,3)

for(h in out){
  fit.temp = glm(duracion ~ tipo1, family=Gamma(link=identity),subset=-h)
  res1=cbind(res1,round(summary(fit.temp)$coef,3))
}
res1

# Comparar los coeficientes con y sin exclusion, calcular cambio porcentual
cbind(
  coef(ajuste3.turbina),
  coef(fit.temp),
  round(100*(coef(ajuste3.turbina)-coef(fit.temp))/coef(ajuste3.turbina),2)
)

####################################################################
# # Ejemplo 2: Espinhel de Fundo                                 # #
####################################################################
# Descripción de los Datos:
# -------------------------
# Una muestra de n = 156 embarcaciones fue analizada durante los años
# 1995 a 1999 siendo 39 de la flota de Ubatuba y 117 de la flota de Santos.
# Las variables consideradas para cada embarcacion fueron:
# - frota (Santos ou Ubatuba),
# - ano (95 a 99),
# - trimestre (1 ao 4),
# - latitude (de 23,25o a 28,25o),
# - longitude(de 41,25o a 50,75o),
# - dias de pesca, captura (quantidade de peixes batata capturados, em kg),
# - cpue (captura por unidade de esforço, kg/dias de pesca).
# Objetivo del Estudio 
# Esplicar la cpue media según las variables frota, ano, trimestre,
# latitude e longitude.
####################################################################

pesca <- read.table(file.choose(), quote="\"", comment.char="")
colnames(pesca)<-c("frota", "ano", "trimestre", "latitude", "longitude", "diaspesca", "captura","cpue")
trimestre = factor(trimestre)
ano = factor(ano)
head(pesca)
attach(pesca)

# Análisis Exploratorio
#-----------------------

plot(density(cpue),ylab="Densidade")
boxplot(split(cpue, frota))
boxplot(split(cpue, ano))

par(mfrow=c(1,2))
boxplot(split(latitude, frota))
boxplot(split(longitude, frota))

par(mfrow=c(1,1))
boxplot(split(cpue, trimestre))

require(robustbase)
# Ver Hubert, M. and Vandervieren, E. (2008)
help(adjbox)

adjbox(split(cpue, frota))
adjbox(split(cpue, ano))

par(mfrow=c(1,2))
adjbox(split(latitude, frota))
adjbox(split(longitude, frota))

par(mfrow=c(1,1))
adjbox(split(cpue, trimestre))

# Dispersion de Cpue vs predictores
par(mfrow=c(1,2))
plot(latitude, cpue, pch=16, xlab="Latitude")
lines(smooth.spline(latitude, cpue, df=3))
plot(longitude, cpue, pch=16, xlab="Longitude")
lines(smooth.spline(longitude, cpue, df=3))

library(psych)
describeBy(cpue[frota == "Santos"],ano[frota == "Santos"]) 
describeBy(cpue[frota == "Ubatuba"],ano[frota == "Ubatuba"]) 


# Conclusiones preliminares:
# - Distribución asimétrica positiva de cpue para cada flota, año y trimestre
# - Mayor cpue para la flota de Santos en relación a la flota de Ubatuba .
# - Pocas diferencias entre los niveles de los factores año y trimestre. 
# - La flota de Santos prefiere latitudes y longitudes mayores que la flota de Ubatuba
# - Indicios de un ligero crecimiento de cpue con latitud aunque con la longitud la tendencia no esta bien definida
# - El supuesto de CV constante parece razonable para la flota de Santos

# Modelo Propuesto
#-----------------------
ajuste1.pesca = glm(cpue ~  frota +  ano + trimestre + latitude + longitude, family=Gamma(link=log))
fit.model <- ajuste1.pesca
summary(fit.model)
source('http://www.poleto.com/funcoes/envel.gama.txt')
envel.gama(fit.model)
source('http://www.poleto.com/funcoes/diag.gama.txt')
diag.gama(fit.model)

# Seleccion de variables
require(MASS)
stepAIC(ajuste1.pesca) 

# Eliminando el factor trimestre por el método de Akaike e incluyendo el factor de interaccion
ajuste2.pesca = glm(cpue ~  frota +  ano + latitude + longitude + frota*ano, family=Gamma(link=log))
ajuste3.pesca = glm(cpue ~  frota +  ano + latitude + longitude, family=Gamma(link=log))
anova(ajuste2.pesca, ajuste3.pesca, test = "F")

summary(ajuste2.pesca)
# Comentarios
# - A medida que aumenta la latitud se espera un aumento del cpue, ocurriendo lo contrario con la longitud
# - Como la interacción es significativa se puede afirmar que la diferencia entre las medias de la cpue de
#   las flotas no es constante a lo largo de los años

# Se confirma la asimetría hacia la derecha
gamma.shape(ajuste2.pesca)

# Diagnóstico
envel.gama(ajuste2.pesca, iden =c(4))
diag.gama(ajuste2.pesca)
diag.gama(ajuste2.pesca, iden=c(2,2,2,0,2,2,3,0))

####################################################################
# Modelo Normal Inversa                                            #
####################################################################
source('invgauss.txt')
#Distribución Normal Inversa para MU = 2 fijo
par(mfrow=c(2,3))
curve(dig(x,mu = 2,lambda = 1),from=0, to=6)
curve(dig(x,mu = 2,lambda = 2),from=0, to=6)
curve(dig(x,mu = 2,lambda = 3),from=0, to=6)
curve(dig(x,mu = 2,lambda = 4),from=0, to=6)
curve(dig(x,mu = 2,lambda = 6),from=0, to=6)
curve(dig(x,mu = 2,lambda = 10),from=0, to=6)
par(mfrow=c(1,1))

####################################################################
# # Ejemplo: Snacks                                              # #
####################################################################
snack = scan("snack.dat", list(cisalhamento=0, grupo=0, semana=0))
attach(snack)
grupo = factor(grupo)

# Análisis descriptivo
boxplot(split(cisalhamento, grupo), xlab="Grupo", ylab="Cisalhamento")
boxplot(split(cisalhamento, semana), xlab="Semanas", ylab="Cisalhamento")
# Boxplots con ajuste para distribuciones asimétricas
require(robustbase)
adjbox(split(cisalhamento, grupo), xlab="Grupo", ylab="Cisalhamento")
adjbox(split(cisalhamento, semana), xlab="Semanas", ylab="Cisalhamento")

s1 = semana
s2 = s1*s1

# Estimación

# Estimación usando el modelo lineal normal
fit0.snack = lm(cisalhamento ~ grupo + s1 + s2)
source('http://www.poleto.com/funcoes/envel.norm.txt')
envel.norm(fit0.snack)
source('http://www.poleto.com/funcoes/diag.norm.txt')
diag.norm(fit0.snack)

# Estimación usando el modelo Normal Inversa
fit1.snack = glm(cisalhamento ~ grupo + s1 + s2,
                 family=inverse.gaussian(link=identity))
summary(fit1.snack)

# Estimación parámetro precisión
length(cisalhamento)/fit1.snack$deviance

# Diagnóstico
#source.with.encoding('envel.ig.R', encoding='ISO-8859-1')
source('http://www.poleto.com/funcoes/invgauss.txt')
source('http://www.poleto.com/funcoes/envel.ig.txt')
envel.ig(fit1.snack,link = "identity")

#source.with.encoding('diag.ig.R', encoding='ISO-8859-1')
source('diag.ig.txt')
diag.ig(fit1.snack)
envel.ig2(fit1.snack, link="identity")

# Estimación usando el modelo Gamma
fit2.snack = glm(cisalhamento ~ grupo + s1 + s2,
                 family=Gamma(link=identity))
summary(fit2.snack)
# Desvío del Modelo
gamma.shape(fit2.snack)$alpha
Dstar<-fit2.snack$deviance*gamma.shape(fit2.snack)$alpha
Dstar
1-pchisq(q = Dstar,df =  fit2.snack$df.residual)

# Diagnóstico
envel.gama(fit2.snack)
diag.gama(fit2.snack)

# Comparación Modelo Gamma y Normal Inversa
rs1<-rstandard(fit1.snack,type="pearson")
rs2<-rstandard(fit2.snack,type="pearson")

par(mfrow=c(1,2))
plot(fit1.snack$fitted.values,rs1,pch=19,ylim=c(-2.5,4.2),
     xlab="Valores Ajustados (Normal Inversa)",
     ylab= "Residuo de Person")
plot(fit2.snack$fitted.values,rs2,pch=19,ylim=c(-2.5,4.2),
     xlab="Valores Ajustados (Gamma)",
     ylab= "Residuo de Person")
par(mfrow=c(1,1))
# Para el modelo Gamma se observa una mayor tendencia sistemática creciente

####################################################################
# # Ejemplo: Seguros                                             # #
####################################################################
car <- read.table("car.csv",sep=",",header=T)
#### Discretización del valor del Vehículo 
valuecat <- cut(car$veh_value, c(-1,2.5,5.0,7.5,10.0,12.5,100))

#### Crear variables con la misma categoría de referencia dada por
#### Jong e Heller (2008)
age.x <- C(factor(car$agecat),base=3) ## agecat=3 nivel base 
area.x <- C(factor(car$area),base=3) ## area C es 3er nivel
gender.x <- C(factor(car$gender),base=2) ## gender M es 2do nivel
veh_body.x <- C(factor(car$veh_body),base=10) ## SEDAN es 10mo nivel 

car <- cbind(car,valuecat, age.x,area.x,gender.x,veh_body.x)

## Ajuste a un modelo Gamma

# Modelo con interacción género y edad
model1 <- glm(claimcst0 ~ age.x + gender.x + age.x*gender.x + area.x + veh_body.x,
              family=Gamma(link="log"),data=subset(car,clm==1))
summary(model1)
gamma.shape(model1)
# Desvío del Modelo
Dstar<-model1$deviance*gamma.shape(model1)$alpha
Dstar
1-pchisq(q = Dstar,df =  model1$df.residual)
# Diagnóstico
envel.gama(model1,sim=20)
diag.gama(model1)

model1b <- glm(claimcst0 ~ age.x + gender.x + area.x,
              family=Gamma(link="log"),data=subset(car,clm==1))
summary(model1b)
gamma.shape(model1b)
# Desvío del Modelo
Dstar<-model1b$deviance*gamma.shape(model1b)$alpha
Dstar
1-pchisq(q = Dstar,df =  model1b$df.residual)


## Ajuste a un modelo Normal Inversa
model2 <- glm(claimcst0 ~ age.x + gender.x + area.x,
              family=inverse.gaussian(link="log"),data=subset(car,clm==1))
summary(model2)

# Estimación parámetro precisión
dim(subset(car,clm==1))[1]/model2$deviance


# Diagnóstico
  envel.ig(model2,sim=20,link="log")
diag.ig(model2,link="log")

