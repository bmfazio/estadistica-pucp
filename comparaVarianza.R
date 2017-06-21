setwd("D:/Clases/puk/muest/datasets")

library(data.table)

load("ce15A.rdata")
Df <- data.table(ce15A)

e <- 5
z <- qnorm(0.975)

K <- nrow(Df)
N <- length(unique(Df$Colegio))

M <- K/N
S <- var(Df[,sum(M500_M),Colegio]$V1/M, na.rm = TRUE)

n1 <- N*S/(S+N*(e/z)**2)
n2 <- (N*S)/((N*(e*M/z)**2)+S)
