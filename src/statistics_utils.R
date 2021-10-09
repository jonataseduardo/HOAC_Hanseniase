library(data.table)
library(ggplot2)
library(insight)
library(ggpubr)
library(lme4)
library(RPostgreSQL) # SGBD PostgreSQL no R

# dependecies tk, gcc-fortran, postgresql-libs (manjaro names pacman)
# install.packages("lme4",
#   repos=c("http://lme4.r-forge.r-project.org/repos",
#          getOption("repos")[["CRAN"]]))
#
#install.packages(c("data.table","ggplot2", "insight", "ggpubr", "ggrepel", "RPostgreSQL"))
#install.packages("tidymodels")

# Cria tabela com valores dos efeitos fixos e aleatórios considerando 
# o modelo de decaimento exponencial. O número inicial de casos é repassado como N0
# e a taxa de decaimento em porcentagem é repassada como R0. 
coef_to_datatable <-
  function(fit, data, nivel){

    if(nivel == 'macro'){
      res <- coef(fit)[["no_macrorregiao:sg_uf"]]
      res1 <- data.table(res)
      new_names <- c('N0', 'R0')
      names(res1) <- new_names
      res1[, c("no_macrorregiao", "sg_uf") := tstrsplit(rownames(res), ":")]
      fit_coef <- 
        data[, .GRP, by = .(no_regiao_brasil, sg_uf, no_macrorregiao )
            ][, .(no_regiao_brasil, sg_uf, no_macrorregiao)
            ][res1, on = .(sg_uf, no_macrorregiao)
            ][, .(no_regiao_brasil, sg_uf, no_macrorregiao, N0, R0)]
    }
    if(nivel == 'uf'){
      res <- coef(fit)[["sg_uf"]]
      res1 <- data.table(res)
      new_names <- c('N0', 'R0')
      names(res1) <- new_names
      res1[, sg_uf := rownames(res)]
      fit_coef <- 
        data[, .GRP, by = .(no_regiao_brasil, sg_uf)
            ][, .(no_regiao_brasil, sg_uf)
            ][res1, on = .(sg_uf)
            ][, .(no_regiao_brasil, sg_uf, N0, R0)]
    }
    if(nivel == 'br'){
      fit_coef <- fixef(fit)|> t() |> data.table()
      new_names <- c('N0', 'R0')
      names(fit_coef) <- new_names
    }
    fit_coef[, ':='(N0 = exp(N0), R0 = 100*R0)]
    return(fit_coef)
  }

# Cria conjunto de dados anuas e de região para predićão
make_newdata <- 
  function(num_cases,
           n_anos,
           groupby = c("sg_uf", "no_macrorregiao", "no_regiao_brasil")
           ){
    if(length(groupby) > 0){
      new_data <-
        replicate(n_anos + 1, 
                  num_cases[, .GRP, by = groupby], 
                  simplify=FALSE) |> 
        rbindlist()
      ngrp = new_data[,.GRP, by = groupby][,.N]
      new_data[, nu_ano := rep(0:n_anos, each = ngrp)]
      new_data[, GRP := NULL]
    }else{
      new_data = data.table(nu_ano = 0:50)
    }
    return(new_data)
  }

#Calcula o número de diagnoticos por regiao
eval_num_cases <-
	function(data, 
           col_usuario, 
           col_populacao,
           groupby = c("no_regiao_brasil", "sg_uf", "no_macrorregiao", "nu_ano")
           ){
    num_cases <- 
      data[, .(NCDR = 100000 * sum(get(col_usuario)) / sum(get(col_populacao)),
               qtd_usuario = sum(get(col_usuario)),
               qtd_populacao = sum(get(col_populacao))
               ), 
          keyby = groupby]
	return(num_cases)
	}

# Faz a predićão de média de diagnósticos e calcula o intervalo de predićão.
fit_ci <- 
  function(fit, new_data, nivel){
    if(nivel == "macro"){
      pred <- predict(fit, newdata=new_data, re.form = NULL, allow.new.levels=TRUE) 
      ci <- 
        get_predicted_ci(fit, 
                         pred, 
                         re.form = NULL, 
                         data = new_data, 
                         ci_type = "prediction") |> data.table()
    }
    if(nivel == "uf"){
      pred <- predict(fit, newdata=new_data, re.form = ~ nu_ano + (1 + nu_ano|sg_uf) , allow.new.levels=TRUE) 
      ci <- 
        get_predicted_ci(fit, 
                         pred, 
                         re.form = ~ nu_ano + (1 + nu_ano|sg_uf), 
                         data = new_data, 
                         ci_type = "prediction") |> data.table()
    }
    if(nivel == "br"){
      pred <- predict(fit, newdata=new_data, re.form = ~0, allow.new.levels=TRUE) 
      ci <- get_predicted_ci(fit, 
                             pred, 
                             re.form = ~0, 
                             data = 
                             new_data, 
                             ci_type = "prediction") |> data.table()
    }
    pred <- pred |> data.table()
    ci_pred  <- cbind(pred, ci)
    ci_pred[, fit := exp(pred)]
    ci_pred[, upr := exp(CI_high)]
    ci_pred[, lwr := exp(CI_low)]
    ci_pred[, c("pred", "CI_low", "CI_high") := NULL]
    fit_data <- cbind(new_data, ci_pred)
    return(fit_data)
  }

# Une os dados brutos com os de predićão
merge_fit_and_data <- 
  function(fit_data, 
           raw_data, 
           keys = c('no_regiao_brasil', 
                    'sg_uf', 
                    'no_macrorregiao', 
                    'nu_ano')
           ){
    return(raw_data[fit_data, on = keys])
  }

# Calcula a projećão e o intervalo de confianca retornando uma tabela 
# com os dados e preojećões e uma segunda tabela com os coeficientes do modelo 
fit_pipeline <- 
  function(data, 
           nivel, 
           col_usuario, 
           col_populacao,
           label){

  num_cases  <- eval_num_cases(data, col_usuario, col_populacao) 
  fit <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano || sg_uf / no_macrorregiao), 
              data = num_cases[NCDR > 0])

  groupby_all = c("no_regiao_brasil", "sg_uf", "no_macrorregiao")
  if(nivel == 'macro'){
    groupby = c("no_regiao_brasil", "sg_uf", "no_macrorregiao")
    new_data <- make_newdata(num_cases, 50, groupby = groupby_all)
    pred_data <- fit_ci(fit, new_data, nivel)
  } 
  if(nivel == 'uf'){
    groupby = c("no_regiao_brasil", "sg_uf")
    new_data <- make_newdata(num_cases, 50, groupby = groupby_all)
    pred_data <- fit_ci(fit, new_data, nivel)
  }
  if(nivel == 'br'){
    groupby = c()
    new_data <- make_newdata(num_cases, 50, groupby = groupby_all)
    pred_data <- fit_ci(fit, new_data, nivel)
  }

  fit_result <- 
    merge_fit_and_data(pred_data, 
                       num_cases, 
                       keys = 
                         c(groupby_all, "nu_ano"))
  fit_result[, grupo := label]
  if(nivel == 'br' || nivel == 'uf'){
    fit_result <- fit_result[, .(NCDR = 100000 * sum(qtd_usuario) / sum(qtd_populacao)),
                          by = c(groupby, c("nu_ano", "SE", "fit", "upr", "lwr", "grupo"))]
  }

  fit_result[, nu_ano := nu_ano + 2001]

  fit_coef <- 
    coef_to_datatable(fit, data, nivel)[, grupo := label]

  return(list(fit_result[], fit_coef[], fit))
  }

# Seleciona regiao e agrupamento de dados para calculo de predićão. 
anlz_pipeline <- 
  function(data, level, grouping){
  if(grouping == 'total'){
    fit_result <- 
        fit_pipeline(data, level, 'qt_usuario', 'qt_populacao', 'Total de Diagnósticos')

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
      fit_pipeline(data, level, 'qt_usuario_20a59', 'qt_populacao20a59', 'entre 15 e 59 anos')
    fit_result_60_00 <- 
      fit_pipeline(data, level, 'qt_usuario_60a00', 'qt_populacao60a00', 'mais que 60 anos')

    fit_data <- 
      rbindlist(list(fit_result_00_14[[1]], fit_result_15_59[[1]], fit_result_60_00[[1]]))

    fit_coef <- 
      rbindlist(list(fit_result_00_14[[2]], fit_result_15_59[[2]], fit_result_60_00[[2]]))

    fit_model <- 
      list(fit_result_00_14[[3]], fit_result_15_59[[3]], fit_result_60_00[[3]])
  }

  if(grouping == 'diag'){
    fit_result_pa <- 
      fit_pipeline(data, level, 'qt_classopera_paucibacilar', 'qt_populacao', 'Paucibacilar')
    fit_result_mu <- 
      fit_pipeline(data, level, 'qt_classopera_multibacilar', 'qt_populacao', 'Multibacilar')

    fit_data <- 
      rbindlist(list(fit_result_pa[[1]], fit_result_mu[[1]]))

    fit_coef <- 
      rbindlist(list(fit_result_pa[[2]], fit_result_mu[[2]]))

    fit_model <- 
      list(fit_result_pa[[1]], fit_result_mu[[1]])
  }

  return(list(fit_data, fit_coef, fit_model))
  }

# Plot dos dados brutos e fit
fit_plot <- 
  function(data){
    g <- 
      ggplot(data) +
        guides(alpha = FALSE) + 
        xlab(label = "Ano") + 
        ylab(label = "Número de novos diagnosticos por 100.000 habitantes") + 
        theme_pubr() + 
        theme(axis.text.x=element_text(angle=45, hjust=1),
              legend.title = element_blank()
              ) + 
        geom_point(aes(x = nu_ano,
                      y = NCDR,
                      color = grupo
                      ),
                  alpha = 0.5,
                  size = 2, 
                  symbol = 3,
                  ) + 
        geom_line(aes(x=nu_ano, 
                      y = fit,
                      color = grupo
                      )
                  )+ 
        geom_ribbon(aes(
                      x=nu_ano, 
                      ymin = lwr, 
                      ymax = upr,
                      fill = grupo
                      ),
                  alpha = 0.1
                  ) 
    return(g)
  }

# Plot dos dados brutos e fit por uf
uf_plot <-
  function(data, uf){
  data_plot <- data[sg_uf == uf]
  g <- 
    fit_plot(data_plot) + 
    facet_wrap(~no_macrorregiao, scales = 'free_y')
  return(g)
  }

# Plot dos dados brutos e fit por Região 
region_plot <- 
  function(data, region){
    data_plot <- data[no_regiao_brasil == region]
    g <- 
      fit_plot(data_plot) + 
      facet_wrap(~sg_uf, scales = 'free_y' )
    return(g)
  }


# Leitura de dados usando PostgreSQL
read_and_preparedata <- 
  function(){
  # conexao com o SGBD
  pgcon <- DBI::dbConnect(
    dbDriver(drvName = "PostgreSQL"),
    dbname = "hanseniase",
    host = "177.85.160.74",
    port = 5432,
    user = 'hans',
    password = "XrtgWA496lf1P4gt"
  )

  data_full <- 
    dbGetQuery(pgcon, "select * from public.vw_hanseniase_categorizado_municipio") |> 
    data.table()

  data <- 
    data_full[!is.na(sg_uf) & !is.na(no_macrorregiao) & nu_ano_dt_notific < 2020]
  data[, nu_ano := nu_ano_dt_notific - min(nu_ano_dt_notific)]
  data[, no_macrorregiao := factor(paste0(no_macrorregiao, ' (', sg_uf, ')'))]
  #data[, no_macrorregiao := gsub("^(Macrorregião|Macrorregional)(.+)$","\\\\2", no_macrorregiao )]
  #data[sg_uf == 'AC', no_macrorregiao := 'Única (AC)']
  data[, co_macrorregiao := factor(co_macrorregiao)]
  return(data)
  }
