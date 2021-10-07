install.packages("performance")
install.packages("see")

---
title: "Sala Hanseníase - Projeção"
output:
  html_document:
    theme: united
    highlight: tango
---

# Apresentação

A seguir é detalhado o passo a passo para realizar as projeções com o modelo LME.

Trata-se de processamento de dados do SINAN, mantido pela SVS em conjunto com o
o DATASUS desenvolvida em linguagem *R*.

# Código-fonte detalhado


Leitura das funções para análise de dados.

```{r eval=F, message=FALSE, warning=FALSE, include=T, paged.print=FALSE}

source("statistics_utils.R")
library(knitr)

```

data <- read_and_preparedata()

# Previsão Total de Diaginósticos

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]

fit_model <- fit_result[[3]]

O número total de diagnósticos de hanseniase no Brasil apresenta uma tendencia
consistente de queda, com uma taxa de queda anual de  - 100 *  fit_coef[, R0] %,
stimada a partir do efeito fixo do modelo.

Para o ano de 2030 é previsto  fit_data[nu_ano == 29, .(fit, lwr, upr)] número
de diagnóticos por 100 mil habitantes, enquanto para o ano de 2050 é previsto 
fit_data[nu_ano == 49, .(fit, lwr, upr)].


g <- fit_plot(fit_data)
g + 
 labs(caption = 
"
Número de diagnóticos de hanseniase por 100 mil habitates em todo território
nacional. Os circulos representam total do diagonósticos feitos no Brasil a
cada ano. Os valores esperados para a número de diagnósticos a cada ano
érepresentado pela linha contínua sendo a área sobreada o intervalo de predição
de 95%
"
 )

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]


### Norte

max_ncdr <- fit_data[no_regiao_brasil == "NORTE" & nu_ano == 18, max(NCDR)]

max_uf <- fit_data[no_regiao_brasil == "NORTE" & nu_ano == 18 & NCDR == max_ncdr, sg_uf]

region_coefs <- 
  data[, .GRP, by = .(no_regiao_brasil, sg_uf)
      ][, .(no_regiao_brasil, sg_uf)
      ][fit_coef, on = .(sg_uf)
      ][no_regiao_brasil == "NORTE"]


Na região Norte o estado que apresenta o maior número de casos detectados no ano de
2019 é o max_uf com max_ncdr diagnósticos por cem mil habitantes, sendo sua a taxa de 
decaimento de novos casos estimado em - 100 * region_coefs[sg_uf == max_uf, R0].

Os interecptos e coeficientes lineares (R0), estimados como efeitos aleatórios do modelo, são:

kable(region_coefs[, .(sg_uf, Intercept, R0 = 100 * R0)], digits=2)

g <- region_plot(fit_data, "NORTE")
g + 
 labs(caption = 
"
Número de diagnóticos de hanseniase por 100 mil habitates na região NORTE. Os
circulos representam total do diagonósticos feitos no Brasil a cada ano. Os
valores esperados para a número de diagnósticos a cada ano érepresentado pela
linha contínua sendo a área sobreada o intervalo de predição de 95%
"
 )

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Tocantins

num_macroregioes <- data[sg_uf == 'TO', length(unique(no_macrorregiao))]

Os interecptos e coeficientes lineares (R0) para as macro regiões do estado do Tocantins são, 
estimados como efeitos aleatórios do modelo, são:

kable(fit_coef[sg_uf == 'TO', .(sg_uf, Intercept, R0 = 100 * R0)], digits=2)

g <- uf_plot(fit_data, "TO")
g + 
 labs(caption = 
"
Número de diagnóticos de hanseniase por 100 mil habitates no estado do
Tocantins. Os circulos representam total do diagonósticos feitos no Brasil a
cada ano. Os valores esperados para a número de diagnósticos a cada ano
érepresentado pela linha contínua sendo a área sobreada o intervalo de predição
de 95%
"
 )


# Previsão  Diaginósticos por sexo

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'sexo')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]
fit_coef

fit_plot(fit_data)

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'sexo')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Norte
region_plot(fit_data, "NORTE")

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'sexo')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Tocantins

uf_plot(fit_data, "TO")

### Mato Grosso

uf_plot(fit_data, "MS")

# Previsão  Diaginósticos por faixa etária

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'idade')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]
fit_plot(fit_data)

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'idade')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Norte
region_plot(fit_data, "NORTE")

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'idade')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Tocantins

uf_plot(fit_data, "TO")

### Mato Grosso

uf_plot(fit_data, "MS")

# Previsão  Diaginósticos por tipo de doenća 

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'diag')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]
fit_plot(fit_data)

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'diag')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Norte
region_plot(fit_data, "NORTE")

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'diag')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Tocantins

uf_plot(fit_data, "TO")

### Mato Grosso

uf_plot(fit_data, "MS")
