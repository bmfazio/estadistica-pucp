# P2 

**a) Bayes**

Dada una variable $Y \sim \text{Bernoulli}(\pi)$, se tiene un modelo de regresión logística si podemos expresar $g(\mathbb E [Y \mid X = x]) = \text{logit}(\text P (Y = 1 \mid X = x))$ en funcion de un predictor lineal $\eta_x$.

Comenzamos expandiendo los terminos al interior de la funcion de enlace:

$$\begin{align}
\text{logit}(\text P (Y = 1 \mid X = x)) &= \log\left(\frac{\text P (Y = 1 \mid X = x)}{\text P (Y = 0 \mid X = x)}\right)\\
&= \log\left(\frac{\text P (X = x \mid Y = 1) \text P (Y = 1)}{\text P (X = x \mid Y = 0) \text P (Y = 0)}\right)\\
&= \log\left(\frac{\text P (Y = 1)}{\text P (Y = 0)}\right) + \log\left(\frac{\text P (X = x \mid Y = 1)}{\text P (X = x \mid Y = 0)}\right)
\end{align}$$

El enunciado nos da las distribuciones de $X \mid Y$ y el parametro $\pi$ para $Y$, reemplazamos:

$$\begin{align}
\text{logit}(\text P (Y = 1 \mid X = x)) &= \log\left(\frac{\pi}{1-\pi}\right) + \log\left(\exp\left[\frac{x^2-(x-\mu)^2}{2}\right]\right)\\
&= \log\left(\frac{\pi}{1-\pi}\right) - \frac{\mu^2}{2} + \mu x
\end{align}$$

Y reconocemos que la expresion toma la forma de un predictor lineal $\eta_x = \beta_0 + \beta_1x$ con

$$\begin{align}
\beta_0 &= \log\left(\frac{\pi}{1-\pi}\right) - \frac{\mu^2}{2}\\
\beta_1 &= \mu
\end{align}$$


# Problema 3 - Simulaciones

  # a) Generar bases de datos

truecoef <- c(-6,0.5,-0.5,0.25,(4:20)*0)

invlogit <- function(x)exp(x)/(1+exp(x))

generarData <- function(n){
  
  X <- matrix(c(rnorm(n, mean = 10, sd = 1),
                rbinom(n, size = 1, prob = 0.5),
                rbeta(n, shape1 = 2, shape2 = 3)),
              nrow = n)
  
  for(i in 4:20){
    X <- cbind(X,rnorm(n, mean = 10, sd = 2))
  }
  
  M <- cbind(rbinom(n, size = 1, prob = invlogit(cbind(1,X)%*%truecoef)),
             X)
  colnames(M) <- c("y",paste0("x",1:20))
  data.frame(M)
  
}

set.seed(911018)
simdata <- lapply(rep(2000,1000),generarData)

  # b) Estimar modelo logistico con restriccion LASSO
cmat.lasso <- matrix(nrow = length(simdata), ncol = 21)

for(i in 1:length(simdata)){
  print(i)
  d <- simdata[[i]]
  cvm <- cv.glmnet(x = as.matrix(d[,-1]), y = d[,1],
                   alpha = 1, # LASSO
                   family = "binomial", type.measure = "auc")
  cmat.lasso[i,] <- as.numeric(coef(cvm,s="lambda.min"))
}

    # Proporcion
apply(cmat.lasso != 0,2,sum)/1000

  # c) Estimar modelo logistico con restriccion ridge
cmat.ridge <- matrix(nrow = length(simdata), ncol = 21)

for(i in 1:length(simdata)){
  print(i)
  d <- simdata[[i]]
  cvm <- cv.glmnet(x = as.matrix(d[,-1]), y = d[,1],
                   alpha = 0, # Ridge
                   family = "binomial", type.measure = "auc")
  cmat.ridge[i,] <- as.numeric(coef(cvm,s="lambda.min"))
}

apply(cmat.ridge != 0,2,sum)/1000

  # d) Estimar sesgo
round(apply(cmat.ridge - matrix(rep(truecoef,1000),ncol=21,byrow = TRUE),2,sum)/1000,4)
round(apply(cmat.lasso - matrix(rep(truecoef,1000),ncol=21,byrow = TRUE),2,sum)/1000,4)
