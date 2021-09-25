


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
  new_data <- 
    replicate(n_anos + 1, 
              num_cases[, .GRP, by = groupby], 
              simplify=FALSE) |> 
    rbindlist()
  ngrp = new_data[,.GRP, by = groupby][,.N]
  new_data[, nu_ano := rep(0:n_anos, each = ngrp)]
  new_data[, GRP := NULL]
  return(new_data)
  }


macro_plot <- 
  function(data, uf){
  ggplot(data[sg_uf == uf]) + 
    guides(alpha = FALSE, fill="none", color=FALSE) + 
    xlab(label = "Anos após 2000") + 
    ylab(label = "Número de novos diagnosticos por 10.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(aes(x = nu_ano, 
                  y = NCDR, 
                  color = sg_uf,
                  ),
              alpha = 0.5,
              size = 1
              ) + 
    geom_line(aes(x=nu_ano, 
                  y = fit, 
                  color = sg_uf)
              )+ 
    geom_ribbon(aes(x=nu_ano, 
                  ymin = lwr, 
                  ymax = upr,
                  fill = sg_uf),
              alpha = 0.2
              ) +
    facet_wrap(~no_macrorregiao)
  }

uf_plot <- 
  function(data, region){
  ggplot(data[no_regiao_brasil == region]) + 
    guides(alpha = FALSE, fill="none", color=FALSE) + 
    xlab(label = "Anos após 2000") + 
    ylab(label = "Número de novos diagnosticos por 10.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(aes(x = nu_ano, 
                  y = NCDR, 
                  color = sg_uf,
                  ),
              alpha = 0.5,
              size = 1
              ) + 
    geom_line(aes(x=nu_ano, 
                  y = fit, 
                  color = sg_uf)
              )+ 
    geom_ribbon(aes(x=nu_ano, 
                  ymin = lwr, 
                  ymax = upr,
                  fill = sg_uf),
              alpha = 0.2
              ) +
    facet_wrap(~sg_uf)
  }

eval_num_cases <-
	function(data, 
           col_usuario, 
           col_populacao,
           groupby = c("no_regiao_brasil", "sg_uf", "no_macrorregiao", "nu_ano")
           ){
	num_cases <- 
	  data[, .(NCDR = 100000 * sum(get(col_usuario)) / sum(get(col_populacao))), 
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
data[, co_macrorregiao := factor(co_macrorregiao)]


#num_cases  <- eval_num_cases(data,"qt_usuario", "qt_populacao")
#num_cases  <- eval_num_cases(data, "qt_classopera_multibacilar", "qt_populacao") 
#num_cases  <- eval_num_cases(data, "qt_usuario_f", "qt_populacao") 
#num_cases  <- eval_num_cases(data, "qt_usuario_m", "qt_populacao") 
#num_cases  <- eval_num_cases(data, "qt_usuario_tpalta_cura", "qt_populacao") 
#num_cases  <- eval_num_cases(data, "qt_usuario_00a14", "qt_populacao00a14") 
num_cases  <- eval_num_cases(data, "qt_usuario_20a59", "qt_populacao20a59") 
#num_cases  <- eval_num_cases(data, "qt_classopera_paucibacilar", "qt_populacao") #all zeros

num_cases[NCDR < 1]

fit_macro <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf / no_macrorregiao), 
             data = num_cases[NCDR > 0])

fit_uf <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf), 
             data = num_cases[NCDR > 0 ])

fit_br <- lm(log(NCDR) ~ nu_ano ,
             data = num_cases[NCDR > 0])

new_data <- make_newdata(num_cases, 50)

macro_pred <- fit_ci(fit_macro, new_data)
macro_data <- merge_fit_and_data(macro_pred, num_cases)

uf_pred <- fit_ci(fit_uf, new_data)
uf_data <- merge_fit_and_data(uf_pred, num_cases)

br_pred <- fit_ci(fit_br, new_data)
br_data <- merge_fit_and_data(br_pred, num_cases)


macro_plot(macro_data, "SP")
uf_plot(uf_data, "SUDESTE")
