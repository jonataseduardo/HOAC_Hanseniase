
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

data0 <- 
  dbGetQuery(pgcon, "select * from public.vw_hanseniase_grupo8999") |> 
  data.table()

# leitura de tabela do banco com SQL
data <- 
  dbGetQuery(pgcon, "select * from public.vw_hanseniase_grupo10000") |> 
  data.table()

#removed NA sg_uf
data <- 
  data[!is.na(sg_uf) & nu_ano < 2020]

num_cases <- 
  data[, .(LogNC = log(.N)), 
       keyby = .(nu_ano, sg_uf, no_regiao_brasil)]

num_cases[, nu_ano := nu_ano - min(nu_ano)]

fit1 <- lmer(LogNC ~ nu_ano + (1 + nu_ano | sg_uf), 
             data = num_cases)

pred_data <- 
  rbindlist(replicate(50, num_cases[, .GRP, .(sg_uf, no_regiao_brasil)], simplify=FALSE))
pred_data[, nu_ano := rep(0:49, each = 27)]
pred_data[, GRP := NULL]


new_data <- predictInterval(fit1, pred_data)  |> data.table()
fit_data <- cbind(pred_data, new_data)


plot(fit1)
qqnorm(resid(fit1))
qqline(resid(fit1))

ggplot(num_cases) + 
  guides(alpha = FALSE, fill=FALSE, color=FALSE) + 
  theme_pubr() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_point(aes(x = nu_ano, 
                y = LogNC, 
                #alpha = sg_uf, 
                color = no_regiao_brasil,
                ),
            size = 2
            ) + 
  geom_line(data =fit_data, 
            aes(x=nu_ano, 
                y = fit, 
                #alpha = sg_uf, 
                color = factor(no_regiao_brasil))
            )+ 
  geom_ribbon(data =fit_data, 
            aes(x=nu_ano, 
                ymin = lwr, 
                ymax = upr,
                #alpha = sg_uf, 
                fill = factor(no_regiao_brasil)),
            alpha = 0.2
            )+ 
  #geom_text_repel(data = num_cases[,.SD[.N], by = .(sg_uf, no_regiao_brasil)], 
  #                aes(x = nu_ano, y = LogNC, label = sg_uf),
  #                min.segment.length = 0,
  #                position = position_nudge_repel(x = 2)
  #                ) + 
  facet_wrap(~sg_uf, ncol= 9)
ggsave("../figures/NC_ano.png", width = 20, height = 20, units = "cm")


summary(model)
