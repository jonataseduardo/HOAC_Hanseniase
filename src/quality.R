library(knitr)
library(performance)
library(ggplot2)
#library(multilevelTools)
#mitl::multilevelR2
#install.packages('partR2')
#library('partR2')

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


group_anova <- 
  function(model_list)
    rbindlist(
      lapply(
        model_list, 
        function(m){
          data.table(anova(m))
        }
        ), 
      idcol = "grupo", 
      use.names = TRUE
    )

lg_anova <-
  function(l, g){
    fit_list <- anlz_pipeline(data, l, g)[[3]]
    res <- group_anova(fit_list)
    res[, modelo := l]
    return(res)
  }



### Main ####

data <- read_and_preparedata()

level_list <- c('uf', 'macro')
group_list <- c('total', 'sexo', 'idade')


l <- level_list[1]
g <- group_list[1]
i <- 1
m <- anlz_pipeline(data, l, g)[[3]][[i]]


for(l in level_list){
  for(g in group_list){

    list_models <- 
      anlz_pipeline(data, l, g)[[3]]

    names_list_models <- 
      names(list_models)

    len <- length(list_models)

    for(i in 1:len){
      m <- anlz_pipeline(data, l, g)[[3]][[i]]
      nm <- names_list_models[i]
      fn <- paste0('../figures/', l, '-', g,'-', nm, '.png')
      print(fn)
      p <- check_model(m)
      print(p)
      ggsave(fn)
      }
    }
  }


performance_results <- 
  rbindlist(lapply(
    level_list, 
    function(l){
      rbindlist(lapply(
        group_list, 
        function(g) {lg_performance(l,g)}
        ))
      }
    ), fill = TRUE, use.names = TRUE)

performance_results


effects_results <- 
  rbindlist(lapply(
    level_list, 
    function(l){
      rbindlist(lapply(
        group_list, 
        function(g) {lg_effects(l,g)}
        ))
      }
    ), fill = TRUE, use.names = TRUE)


anova_results <- 
  rbindlist(lapply(
    level_list, 
    function(l){
      rbindlist(lapply(
        group_list, 
        function(g) {lg_anova(l,g)}
        ))
      }
    ), fill = TRUE, use.names = TRUE)




