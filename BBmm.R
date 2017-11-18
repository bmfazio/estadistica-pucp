setwd("C:/Users/ASUS/Desktop/Hughes-Bayes/_analisisHRoQL/")
# Ejemplo BBmm

library(HRQoL)
library(rootSolve) # Se requiere la funcion multiroot
library(numDeriv)  # Se requiere la funcion grad
source("EffectsEstBBNR.R")
source("VarEst.R")

set.seed(14)

# Defining the parameters

k <- 100            # Numero de observaciones
m <- 10             # Numero de intentos
phi <- 0.5          # Parametro de dispersion
beta <- c(1.5,-1.1) # Coeficientes de predictor lineal
sigma <- 0.5        # Dispersion de los RE


# Simulating the covariate and random effects

x <- runif(k,0,10)             # Generar 100 observaciones unif 0 - 10
X <- model.matrix(~x)          # Producir matriz de diseno FE (1 x)
z <- as.factor(rBI(k,4,0.5,2)) # Generar etiquetas de grupo
Z <- model.matrix(~z-1)        # Producir matriz de diseno para grupos RE
u <- rnorm(5,0,sigma)          # Generar medias de RE para cada grupo


# The linear predictor and simulated response variable

eta <- beta[1]+beta[2]*x+crossprod(t(Z),u) # Generar predictor lineal
p <- 1/(1+exp(-eta))                       # Calcular media (funcion respuesta)
y <- rBB(k,m,p,phi)                        # Generar variable respuesta
dat <- data.frame(cbind(y,x,z))            # Matriz de datos completa
dat$z <- as.factor(dat$z)                  # Convertir a factor

# Apply the model
model <- BBmm(fixed.formula = y~x,random.formula = ~z,m=m,data=dat)
model

##
# Correr BBmm paso por paso
##

function (fixed.formula,         # Parte fija del modelo
          random.formula = NULL, # Parte aleatoria del modelo (A1)
          Z = NULL,              # Matriz de diseno de RE (A2)
          nRandComp = NULL,      # Si hay Z, indicar RE para cada componente
          m,                     # Numero de intentos por observacion binomial
          data = list(),         # datos
          method = "BB-NR",      # metodo estimacion
          maxiter = 100) {       # iteraciones
  

  # Cargamos los nombres de variables usados por la funcion:
  # (agregado por mi)
  fixed.formula  <- y~x  # Parte fija del modelo
  random.formula <- ~z   # Parte aleatoria del modelo (A1)
  Z <- NULL              # Matriz de diseno de RE (A2)
  nRandComp <- NULL      # Si hay Z, indicar RE para cada componente
  m <- m                 # Numero de intentos por observacion binomial
  data <- dat            # datos
  method <- "BB-NR"      # metodo estimacion
  maxiter <- 100         # maximo de iteraciones
  
  # Etapas numeradas de acuerdo a orden en el codigo original #
  
    # 2. Extraer y repartir la informacion relevante para el modelo
    #  X: matriz de diseno
    #  y: outcomes
    #  q, nObs: numero de covariables + const, observaciones
  
  fixed.mf <- model.frame(formula = fixed.formula, data = data)
  X <- model.matrix(attr(fixed.mf, "terms"), data = fixed.mf)
  q <- dim(X)[2]
  y <- model.response(fixed.mf)
  nObs <- length(y)
  
    # 4. Asignando estructura aleatoria
    #   Extrae las siguientes variables
    #   nComp: numero de variables en los RE
    #   nRand: numero total de coeficientes asociados a las variables de los RE
    #   nRandComp: vector con numero de coeficientes por cada variable de los RE
    #   namesRand: nombre de las variables del RE
  
  if (is.null(random.formula)) {
    nComp <- length(nRandComp)
    nRand <- dim(Z)[2]
    namesRand <- as.character(seq(1, nComp, 1))
  }else {
      # Extraer los datos de los componentes aleatorios
    random.mf <- model.frame(formula = update(random.formula, 
                                              ~. - 1), data = data)
    nComp <- dim(random.mf)[2]
    nRandComp <- NULL
    Z <- NULL
    for (i in 1:nComp) {
      z <- model.matrix(~random.mf[, i] - 1)
      Z <- cbind(Z, z)
      nRandComp <- c(nRandComp, dim(z)[2])
    }
    nRand <- dim(Z)[2]
    namesRand <- names(random.mf)
  }

  # 1. Chequeos previos:
  # - Debe especificarse la parte aleatoria, una sola vez
  # - Solo si va Z especificar numero de componentes = ncol(Z)
  # - Seleccionar "BB-NR" o "rootSolve" como metodos de estimacion
  # - Numero de intentos e iteraciones deben ser enteros > 0
  
  if ((is.null(random.formula)) & (is.null(Z))) {
    stop("Random part of the model must be especified")
  }
  if ((is.null(random.formula) == FALSE) & (is.null(Z)) ==FALSE) {
      # Ignora esta advertencia, ocurre porque se genera Z en el bloque anterior
    stop("Random part of the model has been specified twice")
  }
  if ((is.null(Z) == FALSE) & (is.null(nRandComp))) {
    stop("Number of random components must be specified")
  }
  if ((is.null(Z)) & (is.null(nRandComp) == FALSE)) {
    stop("Number of random components must be specified only when Z is defined")
  }
  if (is.null(Z) == FALSE) {
    if (dim(Z)[2] != sum(nRandComp)) {
      stop("The number of random effects in each random component must match with the number of columns of the design matrix Z")
    }
  }
  if ((method == "BB-NR") | (method == "rootSolve")) {
  }else{
    stop("The choosen estamation method is not adequate")
  }
  if (maxiter != as.integer(maxiter)) {
    stop("maxiter must be integer")
  }
  if (maxiter <= 0) {
    stop("maxiter must be positive")
  }
  if (sum(as.integer(m) == m) == length(m)) {
  }else {
    stop("m must be integer")
  }
  if (min(m) <= 0) {
    stop("m must be positive")
  }
  
  # 3. Chequeos adicionales
  
  if (length(m) == 1) {
    balanced <- "yes"
    m. <- rep(m, nObs)
  }else{
    m. <- m
    if (sum(m[1] == m) == length(m)) {
      balanced <- "yes"
    }else {
      balanced <- "no"
    }
  }
  if (sum(as.integer(y) == y) == length(y)) {
  }else {
    stop("y must be integer")
  }
  if ((length(m) == 1) | (length(m) == length(y))) {
  }else {
    stop("m must be a number, or a vector of the length of y")
  }
  if (max(y - m) > 0 | min(y) < 0) {
    stop("y must be bounded between 0 and m")
  }
  
    # 5. Estimacion del modelo
  
  iter <- 0
  
  BB <- BBreg(fixed.formula, m, data) # Regresion beta-binomial solo sobre el componente fijo
  beta <- BB$coefficients             # Extraer coeficientes del modelo
  u <- rep(0, nRand)
  phi <- BB$phi
  all.sigma <- rep(1, nComp)
  d <- d. <- NULL
  for (i in 1:nComp) {                # No entiendo que hacen aqui, all.sigma siempre es 1,
                                      # por que lo invertirian para d.?
    d <- c(d, rep(all.sigma[i], nRandComp[i]))
    d. <- c(d., rep(1/(all.sigma[i]), nRandComp[i]))
  }
  D <- diag(d)                        # Al final lo unico que hacen es generar dos matrices
  D. <- diag(d.)                      # identicas con diagonal 1 x numero de coef aleatorios
  
    # Valores iniciales de los parametros para la estimacion
  oldbetaphisigma <- rep(Inf, q + 1 + nComp)
  betaphisigma <- c(beta, phi, all.sigma)
  
  while (max(abs(betaphisigma - oldbetaphisigma)) > 0.001) {
    oldbetaphisigma <- betaphisigma
    
    if (method == "BB-NR") {
      rand.fix <- EffectsEst.BBNR(y, m., beta, u, p, phi, # Ver descripcion detallada
                                  D., X, Z, maxiter)      # en EffectsEstBBNR.R
      if (rand.fix$conv == "no") {
        print("The method has not converged")
        out <- list(conv = "no")
        return(out)
      }
    }
    else {
      rand.fix <- EffectsEst.multiroot(y, m., beta, u, 
                                       p, phi, D., X, Z, maxiter)
      if (rand.fix$conv == "no") {
        print("The method has not converged")
        out <- list(conv = "no")
        return(out)
      }
    }
    
    beta <- rand.fix$fixed.est
    u <- rand.fix$random.est
    eta <- X %*% beta + Z %*% u
    p <- 1/(1 + exp(-eta))
    effects.iter <- rand.fix$iter
    thetaest <- VarEst(y, m., p, X, Z, u, nRand, nComp, nRandComp, 
                       all.sigma, phi, q, maxiter)
    phi <- thetaest$phi
    all.sigma <- thetaest$all.sigma
    d. <- NULL
    for (i in 1:nComp) {
      d. <- c(d., rep(1/(all.sigma[i]), nRandComp[i]))
    }
    D. <- diag(d.)
    betaphisigma <- c(beta, phi, all.sigma)
    iter <- iter + 1
    cat("Iteration number:", iter, "\\n")
  }
  
  fixed.vcov <- rand.fix$vcov.fixed
  all.sigma.var <- thetaest$all.sigma.var
  psi <- thetaest$psi
  psi.var <- thetaest$psi.var
  fitted.eta <- X %*% beta + Z %*% u
  fitted <- 1/(1 + exp(-(X %*% beta + Z %*% u)))
  conv <- "yes"
  e <- sum(y)/sum(m.)
  l1 <- l2 <- 0
  l1. <- l2. <- 0
  l1.null <- l2.null <- 0
  l3 <- 0
  for (j in 1:nObs) {
    t1 <- 0
    t1. <- 0
    t1.null <- 0
    if (y[j] == 0) {
    }
    else {
      for (k in 0:(y[j] - 1)) {
        t1 <- t1 + log(fitted[j] + k * phi)
        t1. <- t1. + log(y[j]/m.[j] + k * phi)
        t1.null <- t1.null + log(e + k * phi)
      }
    }
    l1 <- l1 + t1
    l1. <- l1. + t1.
    l1.null <- l1.null + t1.null
    t2 <- 0
    t2. <- 0
    t2.null <- 0
    if (y[j] == m.[j]) {
    }
    else {
      for (k in 0:(m.[j] - y[j] - 1)) {
        t2 <- t2 + log(1 - fitted[j] + k * phi)
        t2. <- t2. + log(1 - y[j]/m.[j] + k * phi)
        t2.null <- t2.null + log(1 - e + k * phi)
      }
    }
    l2 <- l2 + t2
    l2. <- l2. + t2.
    l2.null <- l2.null + t2.null
  }
  deviance <- -2 * ((l1 + l2) - (l1. + l2.))
  null.deviance <- -2 * ((l1.null + l2.null) - (l1. + l2.))
  df <- nObs - length(beta) - length(all.sigma) - 1
  null.df <- nObs - 1 - length(all.sigma) - 1
  out <- list(fixed.coef = beta, fixed.vcov = fixed.vcov, random.coef = u, 
              sigma.coef = all.sigma, sigma.var = all.sigma.var, phi.coef = phi, 
              psi.coef = psi, psi.var = psi.var, fitted.values = fitted, 
              conv = conv, deviance = deviance, df = df, null.deviance = null.deviance, 
              null.df = null.df, nRand = nRand, nRandComp = nRandComp, 
              namesRand = namesRand, iter = iter, nObs = nObs, y = y, 
              X = X, Z = Z, D = D, balanced = balanced, m = m, conv = conv)
  class(out) <- "BBmm"
  out$call <- match.call()
  out$formula <- formula
  out
}

######################### original x si la cago

function (fixed.formula, random.formula = NULL, Z = NULL, nRandComp = NULL, 
          m, data = list(), method = "BB-NR", maxiter = 100) {
  if ((is.null(random.formula)) & (is.null(Z))) {
    stop("Random part of the model must be especified")
  }
  if ((is.null(random.formula) == FALSE) & (is.null(Z)) == 
      FALSE) {
    stop("Random part of the model has been specified twice")
  }
  if ((is.null(Z) == FALSE) & (is.null(nRandComp))) {
    stop("Number of random components must be specified")
  }
  if ((is.null(Z)) & (is.null(nRandComp) == FALSE)) {
    stop("Number of random components must be specified only when Z is defined")
  }
  if (is.null(Z) == FALSE) {
    if (dim(Z)[2] != sum(nRandComp)) {
      stop("The number of random effects in each random component must match with the number of columns of the design matrix Z")
    }
  }
  if ((method == "BB-NR") | (method == "rootSolve")) {
  }
  else {
    stop("The choosen estamation method is not adequate")
  }
  if (maxiter != as.integer(maxiter)) {
    stop("maxiter must be integer")
  }
  if (maxiter <= 0) {
    stop("maxiter must be positive")
  }
  if (sum(as.integer(m) == m) == length(m)) {
  }
  else {
    stop("m must be integer")
  }
  if (min(m) <= 0) {
    stop("m must be positive")
  }
  fixed.mf <- model.frame(formula = fixed.formula, data = data)
  X <- model.matrix(attr(fixed.mf, "terms"), data = fixed.mf)
  q <- dim(X)[2]
  y <- model.response(fixed.mf)
  nObs <- length(y)
  if (length(m) == 1) {
    balanced <- "yes"
    m. <- rep(m, nObs)
  }
  else {
    m. <- m
    if (sum(m[1] == m) == length(m)) {
      balanced <- "yes"
    }
    else {
      balanced <- "no"
    }
  }
  if (sum(as.integer(y) == y) == length(y)) {
  }
  else {
    stop("y must be integer")
  }
  if ((length(m) == 1) | (length(m) == length(y))) {
  }
  else {
    stop("m must be a number, or a vector of the length of y")
  }
  if (max(y - m) > 0 | min(y) < 0) {
    stop("y must be bounded between 0 and m")
  }
  if (is.null(random.formula)) {
    nComp <- length(nRandComp)
    nRand <- dim(Z)[2]
    namesRand <- as.character(seq(1, nComp, 1))
  }
  else {
    random.mf <- model.frame(formula = update(random.formula, 
                                              ~. - 1), data = data)
    nComp <- dim(random.mf)[2]
    nRandComp <- NULL
    Z <- NULL
    for (i in 1:nComp) {
      z <- model.matrix(~random.mf[, i] - 1)
      Z <- cbind(Z, z)
      nRandComp <- c(nRandComp, dim(z)[2])
    }
    nRand <- dim(Z)[2]
    namesRand <- names(random.mf)
  }
  iter <- 0
  BB <- BBreg(fixed.formula, m, data)
  beta <- BB$coefficients
  u <- rep(0, nRand)
  phi <- BB$phi
  all.sigma <- rep(1, nComp)
  d <- d. <- NULL
  for (i in 1:nComp) {
    d <- c(d, rep(all.sigma[i], nRandComp[i]))
    d. <- c(d., rep(1/(all.sigma[i]), nRandComp[i]))
  }
  D <- diag(d)
  D. <- diag(d.)
  oldbetaphisigma <- rep(Inf, q + 1 + nComp)
  betaphisigma <- c(beta, phi, all.sigma)
  while (max(abs(betaphisigma - oldbetaphisigma)) > 0.001) {
    oldbetaphisigma <- betaphisigma
    if (method == "BB-NR") {
      rand.fix <- EffectsEst.BBNR(y, m., beta, u, p, phi, 
                                  D., X, Z, maxiter)
      if (rand.fix$conv == "no") {
        print("The method has not converged")
        out <- list(conv = "no")
        return(out)
      }
    }
    else {
      rand.fix <- EffectsEst.multiroot(y, m., beta, u, 
                                       p, phi, D., X, Z, maxiter)
      if (rand.fix$conv == "no") {
        print("The method has not converged")
        out <- list(conv = "no")
        return(out)
      }
    }
    beta <- rand.fix$fixed.est
    u <- rand.fix$random.est
    eta <- X %*% beta + Z %*% u
    p <- 1/(1 + exp(-eta))
    effects.iter <- rand.fix$iter
    thetaest <- VarEst(y, m., p, X, Z, u, nRand, nComp, nRandComp, 
                       all.sigma, phi, q, maxiter)
    phi <- thetaest$phi
    all.sigma <- thetaest$all.sigma
    d. <- NULL
    for (i in 1:nComp) {
      d. <- c(d., rep(1/(all.sigma[i]), nRandComp[i]))
    }
    D. <- diag(d.)
    betaphisigma <- c(beta, phi, all.sigma)
    iter <- iter + 1
    cat("Iteration number:", iter, "\\n")
  }
  fixed.vcov <- rand.fix$vcov.fixed
  all.sigma.var <- thetaest$all.sigma.var
  psi <- thetaest$psi
  psi.var <- thetaest$psi.var
  fitted.eta <- X %*% beta + Z %*% u
  fitted <- 1/(1 + exp(-(X %*% beta + Z %*% u)))
  conv <- "yes"
  e <- sum(y)/sum(m.)
  l1 <- l2 <- 0
  l1. <- l2. <- 0
  l1.null <- l2.null <- 0
  l3 <- 0
  for (j in 1:nObs) {
    t1 <- 0
    t1. <- 0
    t1.null <- 0
    if (y[j] == 0) {
    }
    else {
      for (k in 0:(y[j] - 1)) {
        t1 <- t1 + log(fitted[j] + k * phi)
        t1. <- t1. + log(y[j]/m.[j] + k * phi)
        t1.null <- t1.null + log(e + k * phi)
      }
    }
    l1 <- l1 + t1
    l1. <- l1. + t1.
    l1.null <- l1.null + t1.null
    t2 <- 0
    t2. <- 0
    t2.null <- 0
    if (y[j] == m.[j]) {
    }
    else {
      for (k in 0:(m.[j] - y[j] - 1)) {
        t2 <- t2 + log(1 - fitted[j] + k * phi)
        t2. <- t2. + log(1 - y[j]/m.[j] + k * phi)
        t2.null <- t2.null + log(1 - e + k * phi)
      }
    }
    l2 <- l2 + t2
    l2. <- l2. + t2.
    l2.null <- l2.null + t2.null
  }
  deviance <- -2 * ((l1 + l2) - (l1. + l2.))
  null.deviance <- -2 * ((l1.null + l2.null) - (l1. + l2.))
  df <- nObs - length(beta) - length(all.sigma) - 1
  null.df <- nObs - 1 - length(all.sigma) - 1
  out <- list(fixed.coef = beta, fixed.vcov = fixed.vcov, random.coef = u, 
              sigma.coef = all.sigma, sigma.var = all.sigma.var, phi.coef = phi, 
              psi.coef = psi, psi.var = psi.var, fitted.values = fitted, 
              conv = conv, deviance = deviance, df = df, null.deviance = null.deviance, 
              null.df = null.df, nRand = nRand, nRandComp = nRandComp, 
              namesRand = namesRand, iter = iter, nObs = nObs, y = y, 
              X = X, Z = Z, D = D, balanced = balanced, m = m, conv = conv)
  class(out) <- "BBmm"
  out$call <- match.call()
  out$formula <- formula
  out
}
