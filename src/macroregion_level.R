


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


macro_plot <-
  function(data, uf){
  data_plot <- data[sg_uf == uf]
  ggplot(data_plot) +
    guides(alpha = FALSE) +
    xlab(label = "Ano 20--") + 
    ylab(label = "Número de novos diagnosticos por 100.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(aes(x = nu_ano,
                   y = NCDR,
                   color = grupo
                  ),
              alpha = 0.3,
              size = 1, 
              symbol = 5,
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
              ) +
    facet_wrap(~no_macrorregiao)
  }


uf_plot <- 
  function(data, region){
  data_plot <- data[no_regiao_brasil == region]
  data_m <- 
    data_plot[, .(mean(.SD)), by = .(grupo, sg_uf, nu_ano)]
  ggplot(data_plot) +
    guides(alpha = FALSE, fill="none", color=FALSE) + 
    xlab(label = "Ano 20--") + 
    ylab(label = "Número de novos diagnosticos por 100.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(data = data_m, 
               aes(x = nu_ano,
                   y = NCDR,
                   color = grupo
                  ),
              alpha = 0.9,
              size = 2, 
              symbol = 19,
              ) + 
    geom_point(aes(x = nu_ano,
                   y = NCDR,
                   color = grupo
                  ),
              alpha = 0.2,
              size = 1, 
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
              ) +
    facet_wrap(~sg_uf)
  }

br_plot <- 
  function(data){

  ggplot(data) +
    guides(alpha = FALSE, fill="none", color=FALSE) + 
    xlab(label = "Ano 20--") + 
    ylab(label = "Número de novos diagnosticos por 100.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(aes(x = nu_ano,
                   y = NCDR,
                   color = grupo
                  ),
              alpha = 0.2,
              size = 1, 
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
  function(fit, new_data){
    pred <- predict(fit, newdata=new_data, allow.new.levels=TRUE) 
    ci <- get_predicted_ci(fit, pred, data = new_data, ci_type = "prediction") |> data.table()
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

  #num_cases  <- eval_num_cases(data, col_usuario, col_populacao) 

  if(nivel == 'macro'){
    groupby = c("no_regiao_brasil", "sg_uf", "no_macrorregiao")
    num_cases  <- 
      eval_num_cases(data, 
                     col_usuario, 
                     col_populacao, 
                     groupby = c(groupby, "nu_ano")) 
    fit <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf / no_macrorregiao), 
                data = num_cases[NCDR > 0])
  } 
  if(nivel == 'uf'){
    groupby = c("no_regiao_brasil", "sg_uf")
    num_cases  <- 
      eval_num_cases(data, 
                     col_usuario, 
                     col_populacao, 
                     groupby = c(groupby, "nu_ano")) 
    fit <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf), 
                data = num_cases[NCDR > 0 ])
  }
  if(nivel == 'br'){
    groupby = c()
    num_cases  <- 
      eval_num_cases(data, 
                     col_usuario, 
                     col_populacao, 
                     groupby = c(groupby, "nu_ano")) 
    fit <- lm(log(NCDR) ~ nu_ano ,
              data = num_cases[NCDR > 0])
  }

  new_data <- make_newdata(num_cases, 50, groupby = groupby)
  pred_data <- fit_ci(fit, new_data)
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
#data[, .GRP, no_macrorregiao1]

#num_cases  <- eval_num_cases(data,"qt_usuario", "qt_populacao")


uf_sex <- 
  rbindlist(list(
    pipeline(data, 'uf', 'qt_usuario_m', 'qt_populacao_m', 'm'),
    pipeline(data, 'uf', 'qt_usuario_f', 'qt_populacao_f', 'f')
    ))

macro_sex <- 
  rbindlist(list(
    pipeline(data, 'macro', 'qt_usuario_m', 'qt_populacao_m', 'm'),
    pipeline(data, 'macro', 'qt_usuario_f', 'qt_populacao_f', 'f')
    ))

macro_idade <- 
  rbindlist(list(
    pipeline(data, 'macro', 'qt_usuario_00a14', 'qt_populacao00a14', 'até 14 anos'),
    pipeline(data, 'macro', 'qt_usuario_20a59', 'qt_populacao20a59', 'entre 20 e 59'),
    pipeline(data, 'macro', 'qt_usuario_60a00', 'qt_populacao60a00', 'mais que 60')
    ))

macro_diag <- 
  rbindlist(list(
    pipeline(data, 'macro', 'qt_usuario', 'qt_populacao_m', 'Total'),
    pipeline(data, 'macro', 'qt_classopera_paucibacilar', 'qt_populacao', 'Paucibacilar'),
    pipeline(data, 'macro', 'qt_classopera_multibacilar', 'qt_populacao', 'Multibacilar')
    ))

br_diag <- 
  rbindlist(list(
    #pipeline(data, 'br', 'qt_usuario', 'qt_populacao_m', 'Total'),
    pipeline(data, 'br', 'qt_classopera_paucibacilar', 'qt_populacao', 'Paucibacilar'),
    pipeline(data, 'br', 'qt_classopera_multibacilar', 'qt_populacao', 'Multibacilar')
    ))


br_fit <- pipeline(data, 'br', 'qt_usuario', 'qt_populacao', 'total')
br_total <- eval_num_cases(data, 'qt_usuario', 'qt_populacao', groupby = c('nu_ano'))  
br_total[, grupo := 'total']



br_fit
br_fit[, sum(qtd_populacao), nu_ano]
br_fit[, .(sum(SE)), by = c()]

br_plot(br_fit)

macro_plot(macro_sex, "TO")
macro_plot(macro_idade, "TO")
macro_plot(macro_diag, "TO")

uf_plot(uf_sex, "SUDESTE")
uf_plot(uf_sex, "NORTE")
uf_plot(uf_sex, "SUL")
