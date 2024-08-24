
# ==================================
# Módulo #1. EST079 – MLG
# Ex. 1 – Modelo Linear Normal
# -- Função de produção Cobb-Douglas
# Graduação em Estatística, 2024/1
# Essa versão: 15/05/2024      
# ==================================




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


## carregando dados: Penn World Table (PWT)
## - os dados são carregados no objeto "dta"
dta = read_dta("_dta/pwt1001.dta")
# filtrando para 2010
dta = dta %>% subset(year == '2010')


## visualizando os dados
## (e checando para ver se está tudo OK)
# View(dta)
# checando os países
dta$countrycode
# ordenando por código do país
dta = dta[order(dta$countrycode), ]




# ################## #
# Análise descritiva #
# ################## #

## criando as variáveis de interesse
# definindo as variáveis
dta$Y = dta$rgdpo
dta$L = dta$emp*dta$hc
dta$K = dta$rnna
# rearranjando colunas
dta = dta %>% relocate(Y, .after = year)
dta = dta %>% relocate(L, .after = Y)
dta = dta %>% relocate(K, .after = K)
# filtrando NAs
df.complete = dta[, c('Y', 'L', 'K')] %>% na.omit()
dta = merge(df.complete, dta, all.x = TRUE)

## descritivas básicas
summary(dta[, c('Y', 'L', 'K')])
# salvando em .txt
sink(file = '_out/output/summary_Ex1.txt')
cat('\n')
print(summary(dta[, c('Y', 'L', 'K')]))
sink()


## histograma Y (usando base::plot)
# histograma
hist_data = hist(dta$Y, xlab = 'Y', main = '', col = 'lightblue')
# dist. normal (p/ comparação)
x_values = seq(min(dta$Y), max(dta$Y), length = 100)
y_values = dnorm(x_values, mean = mean(dta$Y), sd = sd(dta$Y)) 
y_values = y_values * diff(hist_data$mids[1:2]) * length(dta$Y) 
lines(x_values, y_values, lwd = 2)


## histograma Y (usando ggplot2)
# invocando ggplot
p = ggplot(dta, aes(x = Y)) +
  # histograma
  geom_histogram(aes(x = Y, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$Y), sd = sd(dta$Y))) + 
  # label eixo x
  xlab('Y') +
  # label eixo y
  ylab('Density') +
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p


## histograma L
# invocando ggplot
p = ggplot(dta, aes(x = L)) +
  # histograma
  geom_histogram(aes(x = L, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$L), sd = sd(dta$L))) + 
  # label eixo x
  xlab('L') +
  # label eixo y
  ylab('Density') +
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p


## histograma K
# invocando ggplot
p = ggplot(dta, aes(x = K)) +
  # histograma
  geom_histogram(aes(x = K, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$K), sd = sd(dta$K))) + 
  # label eixo x
  xlab('K') +
  # label eixo y
  ylab('Density') +
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p


## histograma log(Y)
# invocando ggplot
p = ggplot(dta, aes(x = log(Y))) +
  # histograma
  geom_histogram(aes(x = log(Y), after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(log(dta$Y)), sd = sd(log(dta$Y)))) + 
  # label eixo x
  xlab('log(Y)') +
  # label eixo y
  ylab('Density') +
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.2
dev.print(file = '_out/figures/figEx1_HistDens_log_Y.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## histograma log(L)
# invocando ggplot
p = ggplot(dta, aes(x = log(L))) +
  # histograma
  geom_histogram(aes(x = log(L), after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(log(dta$L)), sd = sd(log(dta$L)))) + 
  # label eixo x
  xlab('log(L)') +
  # label eixo y
  ylab('Density') +
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.2
dev.print(file = '_out/figures/figEx1_HistDens_log_L.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## histograma log(K)
# invocando ggplot
p = ggplot(dta, aes(x = log(K))) +
  # histograma
  geom_histogram(aes(x = log(K), after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(log(dta$K)), sd = sd(log(dta$K)))) + 
  # label eixo x
  xlab('log(K)') +
  # label eixo y
  ylab('Density') +
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.2
dev.print(file = '_out/figures/figEx1_HistDens_log_K.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ################### #
# Análise associativa #
# ################### #

## diagrama de dispersão (usando base::plot)
plot(dta$L, dta$Y,
     xlab = 'L', ylab = 'Y',
     main = 'Diagrama de Dispersão: Y ~ L')
# reta de regressão
abline(lm(dta$Y ~ dta$L), col = 'blue', lwd = 2)
# R2
aux = lm(dta$Y ~ dta$L)
R2 = 1e2*round(cor(dta$Y, aux$fitted.values)^2, 4)
text(x = 50, y = 3e6,
     col = 'red', cex = 1.1,
     labels = paste0('R2 = ', R2, '%')
)


## diagrama de dispersão Y vs. L (usando ggplot2)
# R2
aux = lm(dta$Y ~ dta$L)
R2 = 1e2*round(cor(dta$Y, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = L, y = Y)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('L') +
  # label eixo y
  ylab('Y') + 
  # annotations
  annotate('text', x = 50, y = 3e6, 
           label = paste0('R2 = ', R2, '%'),
           parse = FALSE, fontface = 2, col = 'red') + 
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p


## diagrama de dispersão Y vs. K
# R2
aux = lm(dta$Y ~ dta$K)
R2 = 1e2*round(cor(dta$Y, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = K, y = Y)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('K') +
  # label eixo y
  ylab('Y') + 
  # annotations
  annotate('text', x = 1.75e6, y = 2.75e6, 
           label = paste0('R2 = ', R2, '%'),
           parse = FALSE, fontface = 2, col = 'red') + 
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p


## diagrama de dispersão log(Y) vs. L
# R2
aux = lm(log(dta$Y) ~ dta$L)
R2 = 1e2*round(cor(log(dta$Y), aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = L, y = log(Y))) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('L') +
  # label eixo y
  ylab('log(Y)') + 
  # annotations
  annotate('text', x = 50, y = 18, 
           label = paste0('R2 = ', R2, '%'),
           parse = FALSE, fontface = 2, col = 'red') + 
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p


## diagrama de dispersão log(Y) vs. K
# R2
aux = lm(log(dta$Y) ~ dta$K)
R2 = 1e2*round(cor(log(dta$Y), aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = K, y = log(Y))) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('K') +
  # label eixo y
  ylab('log(Y)') + 
  # annotations
  annotate('text', x = 1.75e6, y = 18, 
           label = paste0('R2 = ', R2, '%'),
           parse = FALSE, fontface = 2, col = 'red') + 
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p


## diagrama de dispersão log(Y) vs. log(L)
# R2
aux = lm(log(dta$Y) ~ log(dta$L))
R2 = 1e2*round(cor(log(dta$Y), aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = log(L), y = log(Y))) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('log(L)') +
  # label eixo y
  ylab('log(Y)') + 
  # annotations
  annotate('text', x = 0, y = log(3e6), 
           label = paste0('R2 = ', R2, '%'),
           parse = FALSE, fontface = 2, col = 'red') + 
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.2
dev.print(file = '_out/figures/figEx1_Scatter_log_Y_log_L.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## diagrama de dispersão log(Y) vs. log(K)
# R2
aux = lm(log(dta$Y) ~ log(dta$K))
R2 = 1e2*round(cor(log(dta$Y), aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = log(K), y = log(Y))) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('log(K)') +
  # label eixo y
  ylab('log(Y)') + 
  # annotations
  annotate('text', x = 10, y = log(2.75e6), 
           label = paste0('R2 = ', R2, '%'),
           parse = FALSE, fontface = 2, col = 'red') + 
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
p
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.2
dev.print(file = '_out/figures/figEx1_Scatter_log_Y_log_K.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ####################################### #
# Modelo Linear Normal (s/ transformação) #
# ####################################### #

## estimação via base::lm
# estimando o modelo
out = lm(Y ~ L + K, data = dta)
# resultados
summary(out)




# ######################## #
# Modelo Normal log-linear #
# ######################## #

## estimação via base::lm
# estimando o modelo
out = lm(log(Y) ~ L + K, data = dta)
# resultados
summary(out)




# ####################### #
# Modelo Normal (log-log) #
# ####################### #

## estimação via base::lm
# estimando o modelo
out = lm(log(Y) ~ log(L) + log(K), data = dta)
# resultados
summary(out)
# salvando em .txt
sink(file = '_out/output/lm_Ex1.txt')
cat('\n')
print(summary(out))
sink()


## estimação manual
# declarando quantidades importantes
n = NROW(dta)
p = 2
log_Y = as.matrix(log(dta$Y))
mX = as.matrix(cbind(rep(1, n), log(dta$L), log(dta$K)))
# EMV de beta
beta.hat = solve(t(mX)%*%mX) %*% t(mX) %*% log_Y
beta.hat


## valores ajustados
log_Y.hat = mX%*%beta.hat

## diagrama de dispersão: log(Y) vs. log_Y.hat
# R2
R2 = 1e2*round(cor(log_Y, log_Y.hat)^2, 4)
# invocando ggplot
plot.obj = ggplot(data = NULL, aes(x = log_Y.hat, y = log_Y)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('hat[log(Y)]') +
  # label eixo y
  ylab('log(Y)') + 
  # annotations
  annotate('text', x = 9, y = 14.75, 
           label = paste0('R2 = ', R2, '%'),
           parse = FALSE, fontface = 2, col = 'red') + 
  # tema do plot
  theme(
    # painel
    panel.background = element_blank(),
    panel.border = element_blank(),
    # eixos
    axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
  )
plot.obj
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.2
dev.print(file = '_out/figures/figEx1_Scatter_log_Y_log_Y_hat.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# #################################### #
# Testes de Hipóteses (modelo log-log) #
# #################################### #

## matriz de variâncias-covariâncias
# -- dos betas (via base::lm)
vcov(out)


## matriz de variâncias-covariâncias
# -- dos betas (manual)
# estimador de sigma2
sigma2.hat = sum((log_Y - log_Y.hat)^2)/(n-(p+1))
sqrt(sigma2.hat)
# var-cov
Sigma = sigma2.hat * solve(t(mX)%*%mX)


## testes de significância (via car::linearHypothesis)
# intercepto
linearHypothesis(out, c('(Intercept) = 0'))
# alpha
linearHypothesis(out, c('log(L) = 0'))
# beta
linearHypothesis(out, c('log(K) = 0'))


## testes de significância (manual)
# intercepto
t.obs = (beta.hat[1] - 0)/sqrt(Sigma[1, 1]); t.obs
2*(1 - pt(abs(t.obs), df = n - (p + 1)))
# alpha
t.obs = (beta.hat[2] - 0)/sqrt(Sigma[2, 2]); t.obs
2*(1 - pt(abs(t.obs), df = n - (p + 1)))
# beta
t.obs = (beta.hat[3] - 0)/sqrt(Sigma[3, 3]); t.obs
2*(1 - pt(abs(t.obs), df = n - (p + 1)))


## testando alpha < 1 e beta < 1 (manual)
## Nota: o car::linearHypothesis não faz testes 
## -- de hipóteses compostas, nem unidirecionais
# alpha
t.obs = (beta.hat[2] - 1)/sqrt(Sigma[2, 2]); t.obs
1 - pt(t.obs, df = n - (p + 1))
# beta
t.obs = (beta.hat[3] - 1)/sqrt(Sigma[3, 3]); t.obs
1 - pt(t.obs, df = n - (p + 1))


## testes de significância conjunta (via car::linearHypothesis)
linearHypothesis(out, c('log(L) = 0', 'log(K) = 0'))


## testes de significância conjunta (manual)
# matriz e vetor de restrições
q = 2
mR = matrix(
  c(0, 1, 0,
    0, 0, 1),
  nrow = q,
  ncol = p + 1,
  byrow = TRUE
)
r = c(0, 0)
# estatística de teste
F.obs = ( t(mR%*%beta.hat - r) %*% solve(sigma2.hat * mR%*%solve(t(mX)%*%mX)%*%t(mR)) %*% (mR%*%beta.hat - r) )/q
F.obs
1 - pf(F.obs, df1 = q, df2 = n - (p+1))


## testando alpha + beta = 1 (via car::linearHypothesis)
linearHypothesis(out, c('log(L) + log(K) = 1'))
# salvando em .txt
sink(file = '_out/output/Ex1_test.txt')
cat('\n')
print(linearHypothesis(out, c('log(L) + log(K) = 1')))
sink()


## testando alpha + beta = 1 (manual)
# matriz e vetor de restrições
q = 1
mR = matrix(
  c(0, 1, 1),
  nrow = q,
  ncol = p + 1,
  byrow = TRUE
)
r = c(1)
# estatística de teste
# F
F.obs = ( t(mR%*%beta.hat - r) %*% solve(sigma2.hat * mR%*%solve(t(mX)%*%mX)%*%t(mR)) %*% (mR%*%beta.hat - r) )/q
F.obs
1 - pf(F.obs, df1 = q, df2 = n - (p+1))
# t (equivalente)
t.obs = (beta.hat[2] + beta.hat[3] - 1)/sqrt(Sigma[2, 2] + Sigma[3, 3] + 2*Sigma[2, 3])
2*(1 - pt(abs(t.obs), df = n - (p + 1)))
t.obs^2

