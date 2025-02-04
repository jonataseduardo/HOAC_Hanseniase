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

library(knitr)
source("statistics_utils.R")

```

data <- read_and_preparedata()

#fwrite(data, '../data/han.csv')

# Previsão Total de casos

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]
"
O número total de casos de hanseníase no Brasil apresenta uma tendência
de queda, com uma taxa anual de  - fit_coef[, R0] %, estimada a partir do
efeito fixo do modelo.
"

"
Para o ano de 2030 é previsto uma taxa de detecção de novos casos por 100 mil habitantes de 
fit_data[nu_ano == 2030, paste0(round(fit, 1), ' - (' , round(lwr, 1), ', ', round(upr, 1), ')CI95%')], 
enquanto para o ano de 2050 é previsto 
fit_data[nu_ano == 2050, paste0(round(fit, 1), ' - (' , round(lwr, 1), ', ', round(upr, 1), ')CI95%')]
novos casos detectados por 100 mil habitantes.
"

g <- 
  fit_plot(fit_data)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes em todo território
nacional. Os círculos representam total do casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%
"
 )

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]


region <- "NORTE"

"
O número total de casos de hanseníase para os estados da região region
apresentam tendencia de queda, com taxas anuais, estimada a partir dos
efeitos aleatórios do modelo, de:
"

kable(fit_coef[no_regiao_brasil == region, 
              .(Estado = sg_uf, "Taxa de queda anual %" = - R0)], 
      digits = 1)

"
A previsão para o número total de casos detectados nos anos de 2030 e 2050 
em cada um dos estados é:
"

kable(
fit_data[no_regiao_brasil == region & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Estado = sg_uf, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = .(Estado, Ano)]
)

g <- region_plot(fit_data, "NORTE")
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes para os estados da
região region. Os círculos representam o número total do casos detectados no
Brasil a cada ano. Os valores esperados para a número de casos a cada
ano representado pela linha contínua sendo a área sombreada o intervalo de
predição de 95%
"
 )

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

uf <- 'TO'

"
O número total de casos de hanseníase para as macrorregiões do  uf 
apresentam tendencia de queda. As taxas anuais de queda, estimada a partir dos
efeitos aleatórios do modelo, são:
"

kable(fit_coef[sg_uf == uf, 
              .(Macrorregião = no_macrorregiao, "Taxa de queda anual %" = - R0)], 
      digits = 1)

"
A previsão para o número total de casos detectados nos anos de 2030 e 2050 
em cada um dos estados é:
"

kable(
fit_data[sg_uf == uf & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Macrorregião = no_macrorregiao, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = .(Macrorregião, Ano)]
)


if(uf %in% c('TO', 'MS', "MT")){
  "
  IMPORTANTE: No estado do uf uma ou mais macrorregiões apresentam tendência de aumento 
  do número de casos recentes. No entanto, os dados do SINAN não possibilitam a investigação
  do motivo do aumento, podendo ser por um crescimento real no número de casos, ou um aumento 
  na eficiência de detecção ou ainda erro no número de casos. 
  "
}

uf <- 'AP'
g <- uf_plot(fit_data, uf)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes nas macrorregiões do
uf. Os círculos representam total do casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%
"
 )


# Previsão para diferentes sexos

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'sexo')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

"
O número de casos de hanseníase no Brasil apresenta uma tendencia
de queda para ambos sexos, sendo a taxa anual para o sexo masculino de  - fit_coef[grupo == "M", R0] %
e para o feminino de - fit_coef[grupo == "F", R0] %. 
Ambas taxas são estimadas a partir do efeitos fixos do modelo.
"

"
A previsão para o número de casos detectados nos anos de 2030 e 2050 
os diferentes sexo é:
"

kable(
fit_data[nu_ano == 2030 | nu_ano == 2050, 
         .(Ano = nu_ano, 
           Sexo = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = .(Sexo, Ano)]
)


g <- 
  fit_plot(fit_data)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes em todo território
nacional. Os círculos representam o número de casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%. 
"
 )

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'sexo')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]


region <- "CENTRO-OESTE"

"
O número de casos de hanseníase para os estados da região region
apresentam tendencia de queda para ambos os sexos. As taxas anuais de queda,
estimada a partir dos efeitos aleatórios do modelo, são:
"

kable(fit_coef[no_regiao_brasil == region, 
              .(Estado = sg_uf, 
                Sexo = grupo, 
                "Taxa de queda anual %" = - R0)
              ][, .SD, keyby = .(Estado, Sexo)], 
      digits = 1)

" 
A previsão para o número de casos detectados para ambos os sexos nos
anos de 2030 e 2050 em cada um dos estados é: 
"

kable(
fit_data[no_regiao_brasil == region & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Estado = sg_uf, 
           Sexo = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = .(Estado, Ano, Sexo )]
)


g <- region_plot(fit_data, region)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes para os estados da
região region. Os círculos representam o número de casos detectados no
Brasil a cada ano. Os valores esperados para a número de casos a cada
ano representado pela linha contínua sendo a área sombreada o intervalo de
predição de 95%
"
 )

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'sexo')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

uf <- 'TO'

"
O número de casos de hanseníase para as macrorregiões do estado do uf 
apresentam tendencia de queda. As taxas anuais de queda, estimada a partir dos
efeitos aleatórios do modelo, são:
"

kable(fit_coef[sg_uf == uf, 
              .(Macrorregião = no_macrorregiao, 
                Sexo = grupo,
                "Taxa de queda anual %" = - R0)
               ][, .SD, keyby = c("Macrorregião", "Sexo")], 
      digits = 1)


" 
A previsão para o número de casos detectados para ambos os sexos nos
anos de 2030 e 2050 nas macrorregiões é: 
"

kable(
fit_data[sg_uf == uf & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Macrorregião = no_macrorregiao, 
                Sexo = grupo,
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = c("Macrorregião", "Ano", "Sexo")]
)


if(uf %in% c('TO', 'MS', "MT")){
  " IMPORTANTE: No estado do uf uma ou mais macrorregiões apresentam tendência
  de aumento do número de casos. No entanto, os dados do SINAN não possibilitam
  a investigação do motivo do aumento, podendo ser por um crescimento real
  no número de casos, ou um aumento na eficiência de detecção ou ainda erro no
  número de casos. "
}

g <- uf_plot(fit_data, uf)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes nas macrorregiões do
uf. Os círculos representam total do casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%
"
 )


# Previsão para diferentes faixas etárias

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'idade')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

"
O número de casos de hanseníase no Brasil apresenta uma tendencia
de queda para diferentes faixas etárias, sendo a taxa anual para crianças de até 14 anos 
- fit_coef[grupo == "até 14 anos", R0] %, 
para jovens e adultos entre 15 e 59 anos - fit_coef[grupo == "entre 15 e 59 anos", R0] % 
 e para idosos com mais de 60 anos - fit_coef[grupo == "mais que 60 anos", R0]. 
Todas as taxas são estimadas a partir do efeitos fixos do modelo.
"

"
A previsão para o número de casos detectados nos anos de 2030 e 2050 
as diferentes faixas etárias é:
"

kable(
fit_data[nu_ano == 2030 | nu_ano == 2050, 
         .(Ano = nu_ano, 
           "Faixa etária" = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = c("Faixa etária", "Ano")]
)


g <- 
  fit_plot(fit_data)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes em todo território
nacional. Os círculos representam o número de casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%. 
"
 )

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'idade')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Norte

region <- "CENTRO-OESTE"

"
O número total de casos de hanseníase para os estados da região region
apresentam tendencia de queda para todas as faixas etarias. As taxas anuais de queda,
estimada a partir dos efeitos aleatórios do modelo, são:
"

kable(fit_coef[no_regiao_brasil == region, 
              .(Estado = sg_uf, 
                "Faixa etária" = grupo, 
                "Taxa de queda anual %" = - R0)
              ][, .SD, keyby = c("Estado", "Faixa etária")], 
      digits = 1)

" 
A previsão para o número de casos detectados cada faixa etária nos
anos de 2030 e 2050 em cada um dos estados é: 
"

kable(
fit_data[no_regiao_brasil == region & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Estado = sg_uf, 
          "Faixa etária" = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = c("Estado", "Ano", "Faixa etária" )]
)


g <- region_plot(fit_data, region)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes para os estados da
região region. Os círculos representam total do casos detectados no
Brasil a cada ano. Os valores esperados para a número de casos a cada
ano representado pela linha contínua sendo a área sombreada o intervalo de
predição de 95%
"
 )

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'idade')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Tocantins

uf <- 'MT'

"
O número total de casos de hanseníase para as macrorregiões do estado do uf 
apresentam tendencia de queda. As taxas anuais de queda, estimada a partir dos
efeitos aleatórios do modelo, são:
"

kable(fit_coef[sg_uf == uf, 
              .(Macrorregião = no_macrorregiao, 
                "Faixa etária" = grupo, 
                "Taxa de queda anual %" = - R0)
               ][, .SD, keyby = c("Macrorregião", "Faixa etária")], 
      digits = 1)


"
A previsão para o número de casos detectados nos anos de 2030 e 2050 
as diferentes faixas etárias é:
"

kable(
fit_data[sg_uf == uf & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Macrorregião = no_macrorregiao, 
          "Faixa etária" = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = c("Macrorregião", "Ano", "Faixa etária")]
)


if(uf %in% c('TO', 'MS', "MT")){
  " IMPORTANTE: No estado do uf uma ou mais macrorregiões apresentam tendência
  de aumento do número de casos principalmente entre idosos. No entanto, os
  dados do SINAN não possibilitam a investigação do motivo do aumento, podendo
  ser por um crescimento real no número de casos, ou um aumento na eficiência
  de detecção ou ainda erro no número de casos. "
}

g <- uf_plot(fit_data, uf)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes nas macrorregiões do
uf. Os círculos representam total do casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%
"
 )


# Previsão para diferentes tipos de casos 

## Brasil

fit_result <- anlz_pipeline(data, 'br', 'diag')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]


"
O número de casos de hanseníase no Brasil apresenta uma tendencia
de queda para os dois tipos de casos, Paucibacilar e Multibacilar.  
A taxa de queda para casos do tipo paucibacilar - fit_coef[grupo == 'Paucibacilar', R0]%  
e para casos do tipo multibacilar é - fit_coef[grupo == 'Paucibacilar', R0]%. 
"

"
A previsão para o número de casos detectados nos anos de 2030 e 2050 
ambos os tipos :
"

kable(
fit_data[nu_ano == 2030 | nu_ano == 2050, 
         .(Ano = nu_ano, 
           "Tipo" = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = c("Tipo", "Ano")]
)


g <- 
  fit_plot(fit_data)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes em todo território
nacional. Os círculos representam o número de casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%. 
"
 )

## Análise por Regiões

fit_result <- anlz_pipeline(data, 'uf', 'diag')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Norte

region <- "CENTRO-OESTE"

"
O número de casos de hanseníase para os estados da região region
apresentam tendencia de queda para ambos os tipos. As taxas anuais de queda,
estimada a partir dos efeitos aleatórios do modelo, são:
"

kable(fit_coef[no_regiao_brasil == region, 
              .(Estado = sg_uf, 
                "Tipo" = grupo, 
                "Taxa de queda anual %" = - R0)
              ][, .SD, keyby = c("Estado", "Tipo")], 
      digits = 1)

"
A previsão para o número de casos detectados nos anos de 2030 e 2050 
ambos os tipos :
"

kable(
fit_data[no_regiao_brasil == region & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Estado = sg_uf, 
          "Tipo" = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = c("Estado", "Ano", "Tipo" )]
)


g <- region_plot(fit_data, region)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes para os estados da
região region. Os círculos representam número de casos detectados no
Brasil a cada ano. Os valores esperados para a número de casos a cada
ano representado pela linha contínua sendo a área sombreada o intervalo de
predição de 95%
"
 )

## Análise por estado

fit_result <- anlz_pipeline(data, 'macro', 'idade')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- fit_result[[3]]

### Tocantins

uf <- 'MT'

"
O número de casos de hanseníase para as macrorregiões do estado do uf 
apresentam tendencia de queda. As taxas anuais de queda, estimada a partir dos
efeitos aleatórios do modelo, são:
"

kable(fit_coef[sg_uf == uf, 
              .(Macrorregião = no_macrorregiao, 
                "Faixa etária" = grupo, 
                "Taxa de queda anual %" = - R0)
               ][, .SD, keyby = c("Macrorregião", "Faixa etária")], 
      digits = 1)


" 
A previsão para o número de casos detectados cada faixa etária nos
anos de 2030 e 2050 em cada um dos estados é: 
"

kable(
fit_data[sg_uf == uf & (nu_ano == 2030 | nu_ano == 2050), 
         .(Ano = nu_ano, 
           Macrorregião = no_macrorregiao, 
          "Faixa etária" = grupo, 
           "NCDR 95%CI" = paste0(round(fit, 1), ' (' , round(lwr, 1), ', ', round(upr, 1), ')'))
         ][, .SD, keyby = c("Macrorregião", "Ano", "Faixa etária")]
)


if(uf %in% c('TO', 'MS', "MT")){
  " IMPORTANTE: No estado do uf uma ou mais macrorregiões apresentam tendência
  de aumento do número de casos principalmente para casos do tipo
  multibacilar. No entanto, os dados do SINAN não possibilitam a investigação
  do motivo do aumento, podendo ser por um crescimento real no número de casos,
  ou um aumento na eficiência de detecção ou ainda erro no número de
  casos. "
}

g <- uf_plot(fit_data, uf)
g + 
 labs(caption = 
"
Número de casos de hanseníase por 100 mil habitantes nas macrorregiões do
uf. Os círculos representam número de casos detectados no Brasil a
cada ano. Os valores esperados para a número de casos a cada ano
é representado pela linha contínua sendo a área sombreada o intervalo de predição
de 95%
"
 )
