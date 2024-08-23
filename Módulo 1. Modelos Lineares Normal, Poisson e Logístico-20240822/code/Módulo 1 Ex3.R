
# ================================
# Módulo #1. EST079 – MLG
# Ex. 3 – Modelo Poisson
# -- Mortalidade geral vs. SES
# Graduação em Estatística, 2024/1
# Essa versão: 15/04/2024      
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


## carregando dados
## - os dados são carregados nos objetos com o prefixo "dta"
## NOTA: o formato desses dados é CSV, mas com separador ";"
# --- SIM
dta.sim = read.csv("_dta/sim [cleaned].csv", sep = ';')
dta.sim$COD_MUNICIP = as.numeric(str_extract(dta.sim$Municipio, "[0-9.]+"))
dta.sim = dta.sim %>% select(COD_MUNICIP, Obitos)
# --- POP
dta.pop = read.csv("_dta/ibge_pop [cleaned].csv", sep = ';')
dta.pop$COD_MUNICIP = as.numeric(str_extract(dta.pop$Municipio, "[0-9.]+"))
dta.pop = dta.pop %>% rename(Populacao = Populacao_residente)
dta.pop = dta.pop %>% select(COD_MUNICIP, Populacao)
# --- PIB
dta.PIB = read.csv("_dta/ibge_pib [cleaned].csv", sep = ';')
dta.PIB$PIB = 
  as.numeric(str_replace(dta.PIB$Produto_interno_bruto_.PIB.., ",", "."))
dta.PIB$COD_MUNICIP = as.numeric(str_extract(dta.PIB$Municipio, "[0-9.]+"))
dta.PIB = dta.PIB %>% select(COD_MUNICIP, PIB)
# --- Gini
dta.gini = read.csv("_dta/ginibr [cleaned].csv", sep = ';')
dta.gini$Gini = 
  as.numeric(str_replace(str_remove(dta.gini$X2010..., "\\,+$"), ",", "."))
dta.gini$COD_MUNICIP = as.numeric(str_extract(dta.gini$Municipio, "[0-9.]+"))
dta.gini = dta.gini %>% select(COD_MUNICIP, Gini)
# --- Desemprego
dta.desemp = read.csv("_dta/ibge_desemp [cleaned].csv", sep = ';')
dta.desemp$Tx_Desemp =
  as.numeric(str_replace(dta.desemp$Taxa_de_desemprego., ",", "."))
dta.desemp$COD_MUNICIP = do.call(rbind, str_extract_all(dta.desemp$Municipio, "[0-9.]+"))
dta.desemp = dta.desemp %>% select(COD_MUNICIP, Tx_Desemp)


## fazendo os merges dos bancos
dta1 = merge(dta.sim, dta.pop, by = 'COD_MUNICIP')
dta2 = merge(dta1, dta.PIB, by = 'COD_MUNICIP')
dta3 = merge(dta2, dta.gini, by = 'COD_MUNICIP')
dta4 = merge(dta3, dta.desemp, by = 'COD_MUNICIP')
dta = dta4
# limpando workspace
rm(dta.sim, dta.pop, dta.PIB, dta.gini, dta.desemp,
   dta1, dta2, dta3, dta4)




# ################## #
# Análise descritiva #
# ################## #

## criando algumas variáveis de interesse
# --- Taxa de Mortalidade (por 100.000 habitantes)
dta$Tx_Mort = 1e5*(dta$Obitos/dta$Populacao)
# --- PIB per capita
dta$PIB_per_capita = dta$PIB/dta$Populacao


## descritivas básicas
summary(dta %>% subset(select = -COD_MUNICIP))
# salvando em .txt
sink(file = '_out/output/summary_Ex3.txt')
cat('\n')
print(summary(dta %>% subset(select = -COD_MUNICIP)))
sink()


## taxa de mortalidade média (global)
## NOTA: essa é diferente da média das taxas!
1e5*(sum(dta$Obitos)/sum(dta$Populacao))


## PIB per Capita médio (global)
1e3*sum(dta$PIB)/sum(dta$Populacao)


## histograma Obitos (usando ggplot2)
# invocando ggplot
p = ggplot(dta, aes(x = Obitos)) +
  # histograma
  geom_histogram(aes(x = Obitos, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$Obitos), sd = sd(dta$Obitos))) + 
  # label eixo x
  xlab('Obitos') +
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


## histograma Populacao
# invocando ggplot
p = ggplot(dta, aes(x = Populacao)) +
  # histograma
  geom_histogram(aes(x = Populacao, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$Populacao), sd = sd(dta$Populacao))) + 
  # label eixo x
  xlab('Populacao') +
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


## histograma PIB (usando ggplot2)
# invocando ggplot
p = ggplot(dta, aes(x = PIB)) +
  # histograma
  geom_histogram(aes(x = PIB, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$PIB), sd = sd(dta$PIB))) + 
  # label eixo x
  xlab('PIB') +
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


## histograma Gini
# invocando ggplot
p = ggplot(dta, aes(x = Gini)) +
  # histograma
  geom_histogram(aes(x = Gini, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$Gini), sd = sd(dta$Gini))) + 
  # label eixo x
  xlab('Gini') +
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
dev.print(file = '_out/figures/figEx3_HistDens_Gini.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## histograma Desemprego
# invocando ggplot
p = ggplot(dta, aes(x = Tx_Desemp)) +
  # histograma
  geom_histogram(aes(x = Tx_Desemp, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$Tx_Desemp), sd = sd(dta$Tx_Desemp))) + 
  # label eixo x
  xlab('Desemp') +
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
dev.print(file = '_out/figures/figEx3_HistDens_Desemp.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## histograma log(Obitos)
# declarando log-Obitos
dta$log_Obitos = log(dta$Obitos)
# invocando ggplot
p = ggplot(dta, aes(x = log_Obitos)) +
  # histograma
  geom_histogram(aes(x = log_Obitos, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$log_Obitos), sd = sd(dta$log_Obitos))) + 
  # label eixo x
  xlab('log(Obitos)') +
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
dev.print(file = '_out/figures/figEx3_HistDens_log_Obitos.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## histograma log(Populacao)
# declarando log-Populacao
dta$log_Populacao = log(dta$Populacao)
# invocando ggplot
p = ggplot(dta, aes(x = log_Populacao)) +
  # histograma
  geom_histogram(aes(x = log_Populacao, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$log_Populacao), sd = sd(dta$log_Populacao))) + 
  # label eixo x
  xlab('log(Populacao)') +
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
dev.print(file = '_out/figures/figEx3_HistDens_log_Populacao.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## histograma log(PIB)
# declarando log-PIB
dta$log_PIB = log(dta$PIB)
# invocando ggplot
p = ggplot(dta, aes(x = log_PIB)) +
  # histograma
  geom_histogram(aes(x = log_PIB, after_stat(density)), fill = 'lightblue', col = 'black', bins = 10) +
  # dist. normal (p/ comparação)
  stat_function(fun = dnorm,
                args = list(mean = mean(dta$log_PIB), sd = sd(dta$log_PIB))) + 
  # label eixo x
  xlab('log(PIB)') +
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
dev.print(file = '_out/figures/figEx3_HistDens_log_PIB.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ################### #
# Análise associativa #
# ################### #

## diagrama de dispersão Obitos vs. Populacao (usando ggplot2)
# R2
aux = lm(Obitos ~ Populacao, data = dta)
R2 = 1e2*round(cor(dta$Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = Populacao, y = Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('Populacao') +
  # label eixo y
  ylab('Obitos') + 
  # annotations
  annotate('text', x = 1.5e6, y = 3e3, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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


## diagrama de dispersão Obitos vs. PIB (usando ggplot2)
# R2
aux = lm(Obitos ~ PIB, data = dta)
R2 = 1e2*round(cor(dta$Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = PIB, y = Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('PIB') +
  # label eixo y
  ylab('Obitos') + 
  # annotations
  annotate('text', x = 6e7, y = 3e3, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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


## diagrama de dispersão Obitos vs. Gini (usando ggplot2)
# R2
aux = lm(Obitos ~ Gini, data = dta)
R2 = 1e2*round(cor(dta$Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = Gini, y = Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('Gini') +
  # label eixo y
  ylab('Obitos') + 
  # annotations
  annotate('text', x = 0.35, y = 3e3, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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


## diagrama de dispersão Obitos vs. Desemprego (usando ggplot2)
# R2
aux = lm(Obitos ~ Tx_Desemp, data = dta)
R2 = 1e2*round(cor(dta$Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = Tx_Desemp, y = Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('Desemprego') +
  # label eixo y
  ylab('Obitos') + 
  # annotations
  annotate('text', x = 3, y = 3e3, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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


## diagrama de dispersão log(Obitos) vs. log(Populacao)
# R2
aux = lm(log_Obitos ~ log_Populacao, data = dta)
R2 = 1e2*round(cor(dta$log_Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = log_Populacao, y = log_Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('log(Populacao)') +
  # label eixo y
  ylab('log(Obitos)') + 
  # annotations
  annotate('text', x = 8.5, y = 7.5, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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
dev.print(file = '_out/figures/figEx3_Scatter_log_Obitos_log_Pop.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## diagrama de dispersão log(Obitos) vs. log(PIB)
# R2
aux = lm(log_Obitos ~ log_PIB, data = dta)
R2 = 1e2*round(cor(dta$log_Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = log_PIB, y = log_Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('log(PIB)') +
  # label eixo y
  ylab('log(Obitos)') + 
  # annotations
  annotate('text', x = 11, y = 7.5, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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
dev.print(file = '_out/figures/figEx3_Scatter_log_Obitos_log_PIB.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## diagrama de dispersão log(Obitos) vs. Gini
# R2
aux = lm(log_Obitos ~ Gini, data = dta)
R2 = 1e2*round(cor(dta$log_Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = Gini, y = log_Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('Gini') +
  # label eixo y
  ylab('log(Obitos)') + 
  # annotations
  annotate('text', x = 0.35, y = 7.5, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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
dev.print(file = '_out/figures/figEx3_Scatter_log_Obitos_Gini.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## diagrama de dispersão log(Obitos) vs. Desemprego
# R2
aux = lm(log_Obitos ~ Tx_Desemp, data = dta)
R2 = 1e2*round(cor(dta$log_Obitos, aux$fitted.values)^2, 4)
# invocando ggplot
p = ggplot(dta, aes(x = Tx_Desemp, y = log_Obitos)) +
  # pontos
  geom_point() +
  # regressão
  geom_smooth(formula = y ~ x, 
              method = 'lm', se = FALSE, 
              col = 'blue', linewidth = 1.25) +
  # label eixo x
  xlab('Desemprego') +
  # label eixo y
  ylab('log(Obitos)') + 
  # annotations
  annotate('text', x = 22, y = 7, 
           label = paste0('R2 = ', format(R2, nsmall = 2), '%'),
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
dev.print(file = '_out/figures/figEx3_Scatter_log_Obitos_Desemp.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ########################### #
# Modelo Poisson (log-linear) #
# ########################### #

## modelos iniciais – estimação via base::glm
# estimando o modelo sem offset
out = glm(Obitos ~ log(Populacao), 
          family = poisson(link = log),
          data = dta)
# resultados
summary(out)


## estimando o modelo sem offset
# (população normalizada por 100.000)
out = glm(Obitos ~ log(Populacao/1e5), 
          family = poisson(link = log),
          data = dta)
# resultados
summary(out)


## estimando o modelo sem offset
# (todos os preditores)
out = glm(Obitos ~ log(Populacao/1e5) + 
            log_PIB + Gini + Tx_Desemp, 
          family = poisson(link = log),
          data = dta)
# resultados
summary(out)


## estimando o modelo *com* offset
# gerando o offset
dta$model.offset = log(dta$Populacao/1e5)
# (todos os preditores)
out = glm(Obitos ~ 
            log_PIB + Gini + Tx_Desemp,
          offset = model.offset,
          family = poisson(link = log),
          data = dta)
# resultados
summary(out)


## modelo "final" – estimação via base::glm
# gerando log(PIB per capita)
dta$log_PIB_per_capita = log(dta$PIB/dta$Populacao)
# estimando o modelo
out = glm(Obitos ~ log_PIB_per_capita + Gini + Tx_Desemp, 
          family = poisson(link = log),
          offset = model.offset,
          data = dta)
# resultados
summary(out)
# salvando em .txt
sink(file = '_out/output/glm_Ex3.txt')
cat('\n')
print(summary(out))
sink()


## riscos relativos
exp(Confint(out))
# salvando em .txt
sink(file = '_out/output/glm_Ex3 (RR).txt')
cat('\n')
print(exp(Confint(out)))
sink()


## estimação da taxa global de mortalidade
# -- via modelo Poisson
out = glm(Obitos ~ 1, 
          family = poisson(link = log),
          offset = model.offset,
          data = dta)
# resultados
summary(out)
print(exp(Confint(out)))

