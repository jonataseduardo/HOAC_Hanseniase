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

#Cria tabela com valores dos efeitos fixos e aleatórios
coef_to_datatable <-
  function(fit, nivel){

    if(nivel == 'macro'){
      res <- coef(fit)[["no_macrorregiao:sg_uf"]]
      res1 <- data.table(res)
      new_names <- c('Intercept', 'R0')
      names(res1) <- new_names
      res1[, c("no_macrorregiao", "sg_uf") := tstrsplit(rownames(res), ":")]
    }
    if(nivel == 'uf'){
      res <- coef(fit)[["sg_uf"]]
      res1 <- data.table(res)
      new_names <- c('Intercept', 'R0')
      names(res1) <- new_names
      res1[, sg_uf := rownames(res)]
    }
    if(nivel == 'br'){
      res1 <- fixef(fit)|> t() |> data.table()
      new_names <- c('Intercept', 'R0')
      names(res1) <- new_names
    }
    return(res1[])
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

# Roda a projećão e o intervalo de confianca retornando uma tabela 
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
  return(list(fit_result[], fit))
  }

# Plot dos dados brutos e fit
fit_plot <- 
  function(data){
    g <- 
      ggplot(data) +
        guides(alpha = FALSE) + 
        xlab(label = "Ano 20--") + 
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
