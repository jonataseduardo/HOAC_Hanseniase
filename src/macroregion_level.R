


#dependecies tk, gcc-fortran, postgresql-libs (manjaro names pacman)
#install.packages("lme4",
#   repos=c("http://lme4.r-forge.r-project.org/repos",
#          getOption("repos")[["CRAN"]]))
#
#install.packages(c("data.table","ggplot2", "insight", "ggpubr", "ggrepel", "RPostgreSQL"))
#install.packages("tidymodels")

library(data.table)
library(ggplot2)
library(insight)
library(ggpubr)
library(lme4)
library(ggrepel)
library(RPostgreSQL) # SGBD PostgreSQL no R


coef_to_DF <-
  function(fit, group){
    res <- coef(fit)[[group]]
    res1 <- data.table(res)
    #new_names <- c('Intercept', 'Coeficient')
    #names(res1) <- new_names
    res1[, (group) := rownames(res)]
    return(res1)
  }

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

uf_plot <-
  function(data, uf){
  data_plot <- data[sg_uf == uf]
  g <- 
    fit_plot(data_plot) + 
    facet_wrap(~no_macrorregiao, scales = 'free_y')
  return(g)
  }

region_plot <- 
  function(data, region){
    data_plot <- data[no_regiao_brasil == region]
    g <- 
      fit_plot(data_plot) + 
      facet_wrap(~sg_uf, scales = 'free_y' )
    return(g)
  }


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
      pred <- predict(fit, newdata=new_data, re.form = ~(1|sg_uf), allow.new.levels=TRUE) 
      ci <- 
        get_predicted_ci(fit, 
                         pred, 
                         re.form = ~(1|sg_uf), 
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
    ci_pred[, upr := fit * exp(CI_high - pred)]
    ci_pred[, lwr := fit * exp(CI_low - pred)]
    ci_pred[, c("pred", "CI_low", "CI_high") := NULL]
    fit_data <- cbind(new_data, ci_pred)
    return(fit_data)
  }

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

pipeline <- 
  function(data, 
           nivel, 
           col_usuario, 
           col_populacao,
           label){

  #Debug example 
  #@col_usuario = 'qt_usuario'
  #@col_populacao = 'qt_populacao'
  #@groupby = c("sg_uf", "no_macrorregiao", "no_regiao_brasil")
  num_cases  <- eval_num_cases(data, col_usuario, col_populacao) 
  fit <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf / no_macrorregiao), 
              data = num_cases[NCDR > 0])

  groupby = c("no_regiao_brasil", "sg_uf", "no_macrorregiao")
  if(nivel == 'macro'){
    #groupby = c("no_regiao_brasil", "sg_uf", "no_macrorregiao")
    new_data <- make_newdata(num_cases, 50, groupby = groupby)
    pred_data <- fit_ci(fit, new_data, nivel)
    #num_cases  <- 
    #  eval_num_cases(data, 
    #                 col_usuario, 
    #                 col_populacao, 
    #                 groupby = c(groupby, "nu_ano")) 
    #fit <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf / no_macrorregiao), 
    #            data = num_cases[NCDR > 0])
  } 
  if(nivel == 'uf'){
    #groupby = c("no_regiao_brasil", "sg_uf")
    new_data <- make_newdata(num_cases, 50, groupby = groupby)
    pred_data <- fit_ci(fit, new_data, nivel)
    #num_cases  <- 
    #  eval_num_cases(data, 
    #                 col_usuario, 
    #                 col_populacao, 
    #                 groupby = c(groupby, "nu_ano")) 
    #fit <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf), 
    #            data = num_cases[NCDR > 0 ])
  }
  if(nivel == 'br'){
    #groupby = c()
    new_data <- make_newdata(num_cases, 50, groupby = groupby)
    pred_data <- fit_ci(fit, new_data, nivel)
    #num_cases  <- 
    #  eval_num_cases(data, 
    #                 col_usuario, 
    #                 col_populacao, 
    #                 groupby = c(groupby, "nu_ano")) 
    #fit <- lm(log(NCDR) ~ nu_ano ,
    #          data = num_cases[NCDR > 0])
  }


  #coef(fit)
  #get_parameters(fit)
  #get_random(fit)

  #new_data <- make_newdata(num_cases, 50, groupby = groupby)
  #pred_data <- fit_ci(fit, new_data)
  fit_result <- merge_fit_and_data(pred_data, num_cases, keys = c(groupby, "nu_ano"))
  fit_result[, grupo := label]
  return(fit_result[])
  }

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


#removed NA sg_uf
data <- 
  data_full[!is.na(sg_uf) & !is.na(no_macrorregiao) & nu_ano_dt_notific < 2020]
data[, nu_ano := nu_ano_dt_notific - min(nu_ano_dt_notific)]
data[, no_macrorregiao := factor(paste0(no_macrorregiao, ' (', sg_uf, ')'))]
data[, no_macrorregiao := gsub("^(Macrorregião|Macrorregional)(.+)$","\\2", no_macrorregiao )]
data[sg_uf == 'AC', no_macrorregiao := 'Única (AC)']
data[, co_macrorregiao := factor(co_macrorregiao)]


# Previsão Total de Diaginósticos
br_total[,NCDR := NULL]
col_usuario = 'qt_usuario'
col_populacao = 'qt_populacao'
br_total
br_total1 <- br_total[, .(NCDR = 100000 * sum(qtd_usuario) / sum(qtd_populacao)),
                      by = .(SE, fit, upr, lwr, grupo, nu_ano)]



fit_plot(br_total1)

br_total <- 
    pipeline(data, 'br', 'qt_usuario', 'qt_populacao', 'Total de Diagnósticos')

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

## Brasil
fit_plot(br_total)

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

