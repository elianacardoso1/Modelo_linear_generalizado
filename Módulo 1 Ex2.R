
# ================================
# Módulo #1. EST079 – MLG
# Ex. 2 – Modelo Poisson
# -- Exemplo sintético
# Graduação em Estatística, 2024/1
# Essa versão: 10/04/2024      
# ================================




# ############ #
# Preliminares #
# ############ #

## limpando o workspace
rm(list = ls())


## carregando pacotes necessários
## Nota: aqui podemos usar tanto 'library' quanto 'require'
# matrizes de correlação
library(corrplot)
# bases de dados diversas
library(haven)
# gráficos diversos
library(ggplot2)
# manipulação de bases de dados
library(tidyverse)
# análise de regressão
library(performance)
library(see)
library(car)


## mudando diretório de trabalho
## Nota: lembre-se de sempre alterar esse caminho!
setwd("C:/Users/uriel/Desktop/Módulo 1/code")


## carregando funções auxiliares
source("_src/src.R")




# ################################# #
# Modelo Poisson: exemplo sintético #
# ################################# #

## simulando o modelo
set.seed(42)
n = 100
beta = 2
x = rnorm(n, mean = 1, sd = 0.05)
eta = x*beta
mu = exp(eta)
y = rpois(n, lambda = mu)


## descritivas básicas
# Y
plot(y)
summary(y)
# mu
plot(mu)
summary(mu)


## função de log-verossimilhança (naive)
loglike = function(beta){
  return(- sum(mu) + sum(y*log(mu)) - sum(log(factorial(y))))
}


## função de log-verossimilhança (menos naive)
loglike = function(sBeta, vX, vY){
  ## declarations
  vEta = vX*sBeta
  vMu = exp(vEta)
  ll = - sum(vMu) + sum(vY*log(vMu)) - sum(lgamma(vY + 1))
  ## return
  return(ll)
}


## função de log-verossimilhança (menos naive ainda)
# --- NOTA: a vetorização aqui é necessária para
# --- plotar a verossimilhança com a função "curve" 
vLoglike = function(vBeta, vX, vY){
  # declarations
  n = length(vY)
  m = length(vBeta)
  ll = rep(NA, m)
  # main loop
  for(j in 1:m){
    vEta = vX*vBeta[j]
    vMu = exp(vEta)
    ll[j] = - sum(vMu) + sum(vY*log(vMu)) - sum(lgamma(vY + 1))
  }
  # return
  return(ll/n)
}


## gráfico da verossimilhança (em função de beta)
vX.global = x
vY.global = y
curve(vLoglike(x, vX = vX.global, vY = vY.global), 
      xlim = c(-4, 4), n = 101, 
      xlab = 'beta', ylab = 'loglike', main = expr(l~(beta~';'~x~','~y)))


## "vetor" score
vS = function(sBeta, vX, vY){
  # declarations
  vEta = vX*sBeta
  vMu = exp(vEta)
  vS.return = matrix(- sum(vX * vMu) + sum(vY * vX))
  # return
  return(vS.return)
}


## "matriz" Hessiana
mH = function(sBeta, vX, vY){
  # declarations
  vEta = vX*sBeta
  vMu = exp(vEta)
  mH.return = matrix(- sum((vX^2) * vMu))
  # return
  return(mH.return)
}


## Newton-Raphson
# globais e gráfico de ell(beta; x, y)
vX.global = x
vY.global = y
curve(vLoglike(x, vX = vX.global, vY = vY.global), 
      xlim = c(-4, 4), n = 101, 
      xlab = 'beta', ylab = 'loglike', main = expr(l~(beta~';'~x~','~y)))
# inicialização
m = 0
beta.0 = 0
vBeta = beta.0
abline(v = vBeta[1], lty = 2)
# iteração m + 1 <- m
m = m + 1; m
mH_beta.m = mH(vBeta[m], vX = vX.global, vY = vY.global)
vS_beta.m = vS(vBeta[m], vX = vX.global, vY = vY.global)
vBeta[m + 1] = vBeta[m] - solve(mH_beta.m)%*%vS_beta.m
vBeta[m + 1]; abline(v = vBeta[m + 1], lty = 2)


## "matriz" de informação de Fisher
mI = loglike = function(sBeta, vX, vY){
  # declarations
  vEta = vX*sBeta
  vMu = exp(vEta)
  mI.return = matrix(sum((vX^2) * vMu))
  # return
  return(mI.return)
}


## Fisher-Scoring
# globais e gráfico de ell(beta; x, y)
vX.global = x
vY.global = y
curve(vLoglike(x, vX = vX.global, vY = vY.global), 
      xlim = c(-4, 4), n = 101, 
      xlab = 'beta', ylab = 'loglike', main = expr(l~(beta~';'~x~','~y)))
# inicialização
m = 0
beta.0 = 0
vBeta = beta.0
abline(v = vBeta[1], lty = 2)
# iteração m + 1 <- m
m = m + 1; m
mI_beta.m = mI(vBeta[m], vX = vX.global, vY = vY.global)
vS_beta.m = vS(vBeta[m], vX = vX.global, vY = vY.global)
vBeta[m + 1] = vBeta[m] + solve(mI_beta.m)%*%vS_beta.m
vBeta[m + 1]; abline(v = vBeta[m + 1], lty = 2)

