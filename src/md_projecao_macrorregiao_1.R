
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

# Brasil


if(grouping == 'total'){
  fit_result <- 
      pipeline(data, level, 'qt_usuario', 'qt_populacao', 'Total de Diagnósticos')

  fit_data <- fit_result[[1]]
  fit_coef <- fit_result[[2]]
  fit_model <- fit_result[[3]]
}

if(grouping == 'sexo'){
  fit_result_m <- 
    fit_pipeline(data, level, 'qt_usuario_m', 'qt_populacao_m', 'M')
  fit_result_f <- 
    fit_pipeline(data, level, 'qt_usuario_f', 'qt_populacao_f', 'F')

  fit_data <- 
    rbindlist(list(fit_result_m[[1]], fit_result_f[[1]]))

  fit_coef <- 
    rbindlist(list(fit_result_m[[2]], fit_result_f[[2]]))

  fit_model <- 
    list(fit_result_m[[3]], fit_result_f[[3]])
}

if(grouping == 'idade'){
  fit_result_00_14 <- 
    fit_pipeline(data, level, 'qt_usuario_00a14', 'qt_populacao00a14', 'até 14 anos')
  fit_result_15_59 <- 
    fit_pipeline(data, level, 'qt_usuario_20a59', 'qt_populacao20a59', 'entre 20 e 59'),
  fit_result_60_00 <- 
    fit_pipeline(data, level, 'qt_usuario_60a00', 'qt_populacao60a00', 'mais que 60')

  fit_data <- 
    rbindlist(list(fit_result_00_14[[1]], fit_result_15_59[[1]], fit_result_59_00[[1]]))

  fit_coef <- 
    rbindlist(list(fit_result_00_14[[2]], fit_result_15_59[[2]], fit_result_59_00[[2]]))

  fit_model <- 
    list(fit_result_00_14[[3]], fit_result_15_59[[3]], fit_result_59_00[[3]])
}

if(grouping == 'diag'){
  fit_result_pa <- 
    fit_pipeline(data, level, 'qt_classopera_paucibacilar', 'qt_populacao', 'Paucibacilar'),
  fit_result_mu <- 
    fit_pipeline(data, level, 'qt_classopera_multibacilar', 'qt_populacao', 'Multibacilar')

  fit_data <- 
    rbindlist(list(fit_result_pa[[1]], fit_result_mu[[1]]))

  fit_coef <- 
    rbindlist(list(fit_result_pa[[1]], fit_result_mu[[1]]))

  fit_model <- 
    list(fit_result_pa[[1]], fit_result_mu[[1]])
}



fit_plot(fit_data)

# Regiao 

uf_total <- 
    pipeline(data, 'uf', 'qt_usuario', 'qt_populacao', 'Total de Diagnósticos')

macro_total <- 
    pipeline(data, 'macro', 'qt_usuario', 'qt_populacao', 'Total de Diagnósticos')

# Previsão diferentes sexos
br_sex <- 
  rbindlist(list(
    pipeline(data, 'br', 'qt_usuario_m', 'qt_populacao_m', 'M'),
    pipeline(data, 'br', 'qt_usuario_f', 'qt_populacao_f', 'F')
    ))

uf_sex <- 
  rbindlist(list(
    pipeline(data, 'uf', 'qt_usuario_m', 'qt_populacao_m', 'M'),
    pipeline(data, 'uf', 'qt_usuario_f', 'qt_populacao_f', 'F')
    ))

macro_sex <- 
  rbindlist(list(
    pipeline(data, 'macro', 'qt_usuario_m', 'qt_populacao_m', 'M'),
    pipeline(data, 'macro', 'qt_usuario_f', 'qt_populacao_f', 'F')
    ))

# Previsão diferentes idades

br_idade <- 
  rbindlist(list(
    pipeline(data, 'br', 'qt_usuario_00a14', 'qt_populacao00a14', 'até 14 anos'),
    pipeline(data, 'br', 'qt_usuario_20a59', 'qt_populacao20a59', 'entre 20 e 59'),
    pipeline(data, 'br', 'qt_usuario_60a00', 'qt_populacao60a00', 'mais que 60')
    ))

uf_idade <- 
  rbindlist(list(
    pipeline(data, 'uf', 'qt_usuario_00a14', 'qt_populacao00a14', 'até 14 anos'),
    pipeline(data, 'uf', 'qt_usuario_20a59', 'qt_populacao20a59', 'entre 20 e 59'),
    pipeline(data, 'uf', 'qt_usuario_60a00', 'qt_populacao60a00', 'mais que 60')
    ))

macro_idade <- 
  rbindlist(list(
    pipeline(data, 'macro', 'qt_usuario_00a14', 'qt_populacao00a14', 'até 14 anos'),
    pipeline(data, 'macro', 'qt_usuario_20a59', 'qt_populacao20a59', 'entre 20 e 59'),
    pipeline(data, 'macro', 'qt_usuario_60a00', 'qt_populacao60a00', 'mais que 60')
    ))

# Previsão dieferentes diagnosticos 

br_diag <- 
  rbindlist(list(
    pipeline(data, 'br', 'qt_classopera_paucibacilar', 'qt_populacao', 'Paucibacilar'),
    pipeline(data, 'br', 'qt_classopera_multibacilar', 'qt_populacao', 'Multibacilar')
    ))

uf_diag <- 
  rbindlist(list(
    pipeline(data, 'uf', 'qt_classopera_paucibacilar', 'qt_populacao', 'Paucibacilar'),
    pipeline(data, 'uf', 'qt_classopera_multibacilar', 'qt_populacao', 'Multibacilar')
    ))

macro_diag <- 
  rbindlist(list(
    pipeline(data, 'macro', 'qt_classopera_paucibacilar', 'qt_populacao', 'Paucibacilar'),
    pipeline(data, 'macro', 'qt_classopera_multibacilar', 'qt_populacao', 'Multibacilar')
    ))

# Total


## Região
region_plot(uf_total, "NORTE")

## Estado
uf_plot(macro_total, "TO")


fit_plot(br_sex)
fit_plot(br_idade)
fit_plot(br_diag)

region_plot(uf_sex, "NORTE")
region_plot(uf_idade, "NORTE")
region_plot(uf_diag, "NORTE")

uf_plot(macro_total, "MG")
uf_plot(macro_sex, "TO")
uf_plot(macro_idade, "TO")
uf_plot(macro_diag, "MT")

