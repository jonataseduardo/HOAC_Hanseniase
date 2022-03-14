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

gl_performance <-
  function(l, g){
    fit_list <- anlz_pipeline(data, l, g)[[3]]
    res <- group_performance(fit_list)
    res[, modelo := l]
    return(res)
  }

data <- read_and_preparedata()

#m1 <- anlz_pipeline(data, 'uf', 'total')[[3]][[1]]

level_list <- c('uf', 'macro')
group_list <- c('total', 'sexo', 'idade')


pres <- 
  rbindlist(lapply(
    level_list, 
    function(l){
      rbindlist(lapply(
        group_list, 
        function(g) {gl_performance(l,g)}
        ))
      }
    ), fill = TRUE, use.names = TRUE)


pres[, .SD, by = .(grupo, modelo)]

names(data)
