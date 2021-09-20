
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
  function(num_cases, n_anos){
  new_data <- 
    replicate(n_anos + 1, 
              num_cases[, .GRP, .(sg_uf, co_macrorregiao, no_regiao_brasil)], 
              simplify=FALSE) |> 
    rbindlist()
  ngrp = new_data[,.GRP, .(sg_uf, co_macrorregiao, no_regiao_brasil)][,.N]
  new_data[, nu_ano := rep(0:n_anos, each = ngrp)]
  new_data[, GRP := NULL]
  return(new_data)
  }

fit_ci <- 
  function(fit, new_data){
    pred <- predict(fit, newdata=new_data) 
    ci <- get_predicted_ci(fit, pred, data = new_data, ci_type = "prediction") |> data.table()
    pred <- pred |> data.table()
    ci_pred  <- cbind(pred, ci)
    setnames(ci_pred, c('pred', 'CI_low', 'CI_high'), c('fit', 'lwr', 'upr'))
    fit_data <- cbind(new_data, ci_pred)
    return(fit_data)
  }

macro_plot <- 
  function(num_cases, fit_data, uf){
  ggplot(num_cases[sg_uf == uf]) + 
    guides(alpha = FALSE, fill="none", color=FALSE) + 
    xlab(label = "Anos após 2000") + 
    ylab(label = "Número de novos diagnosticos por 10.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(aes(x = nu_ano, 
                  y = log(NCDR), 
                  #alpha = uf, 
                  color = factor(co_macrorregiao),
                  ),
              size = 2
              ) + 
    geom_line(data=fit_data[sg_uf == uf], 
              aes(x=nu_ano, 
                  y = fit, 
                  #alpha = sg_uf, 
                  color = factor(co_macrorregiao))
              )+ 
    geom_ribbon(data =fit_data[sg_uf == uf], 
              aes(x=nu_ano, 
                  ymin = lwr, 
                  ymax = upr,
                  #alpha = sg_uf, 
                  fill = factor(co_macrorregiao)),
              alpha = 0.2
              ) +
    facet_wrap(~co_macrorregiao)
    #ggsave(paste0("../figures/NC_", region, ".png"), width = 20, height = 20, units = "cm")
  }

uf_plot <- 
  function(num_cases, fit_data, region){
  ggplot(num_cases[no_regiao_brasil == region]) + 
    guides(alpha = FALSE, fill="none", color=FALSE) + 
    xlab(label = "Anos após 2000") + 
    ylab(label = "Número de novos diagnosticos por 10.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(aes(x = nu_ano, 
                  y = log(NCDR), 
                  color = sg_uf,
                  ),
              alpha = 0.5,
              size = 1
              ) + 
    geom_line(data=fit_data[no_regiao_brasil == region], 
              aes(x=nu_ano, 
                  y = fit, 
                  color = sg_uf)
              )+ 
    geom_ribbon(data =fit_data[no_regiao_brasil == region], 
              aes(x=nu_ano, 
                  ymin = lwr, 
                  ymax = upr,
                  fill = sg_uf),
              alpha = 0.2
              ) +
    #geom_text_repel(data = num_cases[,.SD[.N], by = .(sg_uf, no_regiao_brasil)], 
    #                aes(x = nu_ano, y = LogNC, label = sg_uf),
    #                min.segment.length = 0,
    #                position = position_nudge_repel(x = 2)
    #                ) + 
    facet_wrap(~sg_uf)
  #ggsave(paste0("../figures/NC_", region, ".png"), width = 20, height = 20, units = "cm")
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

data <- 
  dbGetQuery(pgcon, "select * from public.vw_hanseniase_categorizado_municipio") |> 
  data.table()

#removed NA sg_uf
data <- 
  data[!is.na(sg_uf) & !is.na(co_macrorregiao) & nu_ano_dt_notific < 2020]
data[, co_macrorregiao := factor(co_macrorregiao)]
data[, nu_ano := nu_ano_dt_notific - min(nu_ano_dt_notific)]

#num_cases <- 
#  data[, .(NCDR = 10000 * sum(qt_usuario) / sum(qt_populacao)), 
#       keyby = .(nu_ano, sg_uf, co_macrorregiao, no_regiao_brasil)]

num_cases <- 
  data[, .(NCDR = 10000 * sum(qt_usuario) / sum(qt_populacao)), 
       keyby = .(nu_ano, sg_uf, co_macrorregiao, no_regiao_brasil)]

fit_macro <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf / co_macrorregiao), 
             data = num_cases)

fit_uf <- lmer(log(NCDR) ~ nu_ano + (1 + nu_ano | sg_uf), 
             data = num_cases)

new_data <- make_newdata(num_cases, 50)

macro_pred <- fit_ci(fit_macro, new_data)

uf_pred <- fit_ci(fit_uf, new_data)

macro_plot(num_cases, macro_pred, "SP")
uf_plot(num_cases, uf_pred, "SUDESTE")

#for(region in num_cases[,.GRP, by = no_regiao_brasil][, no_regiao_brasil]){
#  uf_plot(region)
#}


