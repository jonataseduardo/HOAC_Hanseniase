library(knitr)
library(performance)
install.packages('devtools')
library(devtools)
library(r2mlm)
devtools::install_github("mkshaw/r2mlm")

devtools::install_github("strengejacke/sjPlot")

source("statistics_utils.R")

data <- read_and_preparedata()

fit_result <- anlz_pipeline(data, 'macro', 'total')
fit_data <- fit_result[[1]]
fit_coef <- fit_result[[2]]
fit_model <- anlz_pipeline(data, 'uf', 'total')[[3]]

m1 <- anlz_pipeline(data, 'uf', 'total')[[3]][[1]]
m2 <- anlz_pipeline(data, 'macro', 'total')[[3]][[1]]

x <- rbindlist(lapply(fit_model, model_performance, metrics = 'all'), use.names = TRUE)

test_performance(m1, m2)
r2(m21 tolerance= 1e-4)

r2mlm(m2, bargraph=FALSE)

z <- 
  anova(m2)

show_tests(z)
z
z <- drop1(m2)
z


zz <- data.table(z)
zz[, level := c(0, names(attr(z, "formulae")))]
zz

data(TVbo)

tv <- 
  data.table(TVbo)

tv[, .GRP, .(Assessor, TVset, Repeat, Picture)]
tv[, .GRP, .(Assessor, TVset)]
