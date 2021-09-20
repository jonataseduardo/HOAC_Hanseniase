


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


# leitura de tabela do banco com SQL
data <- 
  dbGetQuery(pgcon, "select * from public.vw_hanseniase_categorizado_municipio") |> 
  data.table()

data[,qt_populacao := qt_populacao00a19 + qt_populacao20a59 + qt_populacao60a00]
data[, nu_ano := nu_ano_dt_notific - min(nu_ano_dt_notific)]
data[, co_macrorregiao := factor(co_macrorregiao)]

#removed NA sg_uf
data <- 
  data[!is.na(sg_uf) & nu_ano < 2020]

data <- data[!is.na(co_macrorregiao)]

num_cases <- 
  data[, .(NC = log(10000 * sum(qt_usuario) / sum(qt_populacao))),
       by = .(nu_ano, sg_uf, co_macrorregiao, no_regiao_brasil)]

fit1 <- lmer(NC ~ nu_ano + (1 + nu_ano | sg_uf/co_macrorregiao ),
             data = num_cases,
             REML = FALSE
)

coef(fit1)
plot(fit1)
qqnorm(resid(fit1))
qqline(resid(fit1))

x <- ggpredict(fit1, terms = 'nu_ano') 
x
|> data.table()
str(x)
x$nu_ano$x



pred_data <- 
  rbindlist(replicate(50, num_cases[, .GRP, .(sg_uf, no_regiao_brasil)], simplify=FALSE))

ngrp = num_cases[, .GRP, sg_uf][,.N]
pred_data[, nu_ano := rep(0:49, each = ngrp)]
pred_data[, GRP := NULL]


pred_data <- 
  rbindlist(replicate(50, num_cases[, .GRP, .(co_macrorregiao, sg_uf, no_regiao_brasil)], simplify=FALSE))

ngrp = num_cases[, .GRP, co_macrorregiao][,.N]
pred_data[, nu_ano := rep(0:49, each = ngrp)]
pred_data[, GRP := NULL]

new_data <- predictInterval(fit1, pred_data)  |> data.table()
new_data1 <- add_ci(fit1, pred_data)  |> data.table()
fit_data <- cbind(pred_data, new_data)


ggplot(num_cases[sg_uf == 'RJ']) + 
  guides(alpha = FALSE, fill=FALSE, color=FALSE) + 
  theme_pubr() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_point(aes(x = nu_ano, 
                y = NC, 
                #alpha = sg_uf, 
                color = factor(co_macrorregiao),
                ),
            size = 2
            ) + 
  geom_line(data =fit_data[sg_uf == 'RJ'], 
            aes(x=nu_ano, 
                y = fit, 
                #alpha = sg_uf, 
                color = factor(co_macrorregiao))
            )+ 
  geom_ribbon(data =fit_data[sg_uf == 'RJ'], 
            aes(x=nu_ano, 
                ymin = lwr, 
                ymax = upr,
                #alpha = sg_uf, 
                fill = factor(co_macrorregiao)),
            alpha = 0.2
            )
  + 
  #geom_text_repel(data = num_cases[,.SD[.N], by = .(sg_uf, no_regiao_brasil)], 
  #                aes(x = nu_ano, y = LogNC, label = sg_uf),
  #                min.segment.length = 0,
  #                position = position_nudge_repel(x = 2)
  #                ) + 
  facet_wrap(~sg_uf, ncol= 9)
ggsave("../figures/NC_ano.png", width = 20, height = 20, units = "cm")


summary(model)
