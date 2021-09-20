
library(data.table)
library(ggplot2)
library(ggpubr)
library(lme4)
library(merTools)
library(ggeffects)
library(ggrepel)
library(RPostgreSQL) # SGBD PostgreSQL no R
library(parameters)


coef_to_DF <-
  function(fit, group){
    res <- coef(fit)[[group]]
    res1 <- data.table(res)
    #new_names <- c('Intercept', 'Coeficient')
    #names(res1) <- new_names
    res1[, (group) := rownames(res)]
    return(res1)
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
  data[!is.na(sg_uf) & nu_ano_dt_notific < 2020]

data[, nu_ano := nu_ano_dt_notific - min(nu_ano_dt_notific)]

num_cases <- 
  data[, .(LogNC = log(10000 * sum(qt_usuario) / sum(qt_populacao))), 
       keyby = .(nu_ano, sg_uf, no_regiao_brasil)]


num_cases[, NCDR := exp(LogNC)]

fit1 <- lmer(LogNC ~ nu_ano + (1 + nu_ano | sg_uf), 
             data = num_cases)

pred_data <- 
  rbindlist(replicate(50, num_cases[, .GRP, .(sg_uf, no_regiao_brasil)], simplify=FALSE))
pred_data[, nu_ano := rep(0:49, each = 27)]
pred_data[, GRP := NULL]


new_data <- predictInterval(fit1, pred_data, include.resid.var = TRUE)  |> data.table()
fit_data <- cbind(pred_data, new_data)


plot(fit1)
qqnorm(resid(fit1))
qqline(resid(fit1))

fit_data[,`:=`(fit = exp(fit), upr = exp(upr), lwr = exp(lwr))]

state_plot <- 
  function(region){
  ggplot(num_cases[no_regiao_brasil == region]) + 
    guides(alpha = FALSE, fill=FALSE, color=FALSE) + 
    xlab(label = "Anos após 2000") + 
    ylab(label = "Número de novos diagnosticos por 10.000 habitantes") + 
    theme_pubr() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    geom_point(aes(x = nu_ano, 
                  y = NCDR, 
                  #alpha = sg_uf, 
                  color = no_regiao_brasil,
                  ),
              size = 2
              ) + 
    geom_line(data =fit_data[no_regiao_brasil == region], 
              aes(x=nu_ano, 
                  y = fit, 
                  #alpha = sg_uf, 
                  color = factor(no_regiao_brasil))
              )+ 
    geom_ribbon(data =fit_data[no_regiao_brasil == region], 
              aes(x=nu_ano, 
                  ymin = lwr, 
                  ymax = upr,
                  #alpha = sg_uf, 
                  fill = factor(no_regiao_brasil)),
              alpha = 0.2
              ) + 
    #geom_text_repel(data = num_cases[,.SD[.N], by = .(sg_uf, no_regiao_brasil)], 
    #                aes(x = nu_ano, y = LogNC, label = sg_uf),
    #                min.segment.length = 0,
    #                position = position_nudge_repel(x = 2)
    #                ) + 
    facet_wrap(~sg_uf)
  ggsave(paste0("../figures/NC_", region, ".png"), width = 20, height = 20, units = "cm")
  }


for(region in num_cases[,.GRP, by = no_regiao_brasil][, no_regiao_brasil]){
  state_plot(region)
}


summary(model)
