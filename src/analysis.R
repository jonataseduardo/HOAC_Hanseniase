library(knitr)
library(performance)

source("statistics_utils.R")

group_performance <- 
  function(model_list)
    rbindlist(
      lapply(
        model_list, 
        model_performance, 
        metrics = 'all'
        ), 
      idcol = "grupo", 
      use.names = TRUE
    )

lg_performance <-
  function(l, g){
    fit_list <- anlz_pipeline(data, l, g)[[3]]
    res <- group_performance(fit_list)
    res[, modelo := l]
    return(res)
  }


group_effects <- 
  function(model_list)
    rbindlist(
      lapply(
        model_list, 
        function(m){
          y <- data.table(coef(summary(m)))
          y[, 'Fixed Effect' := c('Intercept', 'Ano')]
          y[, .SD, by = c('Fixed Effect')]
          return(y)
        }
        ), 
      idcol = "grupo", 
      use.names = TRUE
    )

lg_effects <-
  function(l, g){
    fit_list <- anlz_pipeline(data, l, g)[[3]]
    res <- group_effects(fit_list)
    res[, modelo := l]
    return(res)
  }

data <- read_and_preparedata()


level_list <- c('uf', 'macro')
group_list <- c('total', 'sexo', 'idade')

l <- level_list[1]
g <- group_list[1]

lg_effects(l, g)


pref <- 
  rbindlist(lapply(
    level_list, 
    function(l){
      rbindlist(lapply(
        group_list, 
        function(g) {lg_performance(l,g)}
        ))
      }
    ), fill = TRUE, use.names = TRUE)

eres <- 
  rbindlist(lapply(
    level_list, 
    function(l){
      rbindlist(lapply(
        group_list, 
        function(g) {lg_effects(l,g)}
        ))
      }
    ), fill = TRUE, use.names = TRUE)

eres

pres[, .SD, by = .(grupo, modelo)]

m1 <- anlz_pipeline(data, 'uf', 'total')[[3]][[1]]
m2 <- anlz_pipeline(data, 'macro', 'total')[[3]][[1]]

x <- anova(m1)
data.table(x)

y <- data.table(coef(summary(m2)))
y[, 'Fixed Effect' := c('Intercept', 'Ano')]
y[, .SD, by = c('Fixed Effect')]



