
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

Bibliotecas utilizadas.

source("statistics_utils.R")

data <- read_and_preparedata()

# Previsão Total de Diaginósticos

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]
fit_plot(fit_data)

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Norte
region_plot(fit_data, "NORTE")

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Tocantins

uf_plot(fit_data, "TO")

### Mato Grosso

uf_plot(fit_data, "MS")


# Previsão  Diaginósticos por sexo

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'sexo')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]
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
