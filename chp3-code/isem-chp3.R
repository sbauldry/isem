### Purpose: Prepare Chapter 3 examples
### Author:  S Bauldry
### Date:    Feb 4, 2025

setwd("~/desktop")
library(tidyverse)
library(lavaan)
library(MIIVsem)
library(ggpubr)


### Load prepared NSDUH data
d1 <- read_csv("isem-nsduh-data.csv")


### complete case data
d2 <- d1 |>
  na.omit()


### Figure 3.4 -- distribution of indicators
genfig <- function(var, vlab) {
  v <- rlang::sym(var)
  f <- ggplot(data = d2, mapping = aes(x = factor(!!v))) +
    geom_bar(aes(y = after_stat(prop), group = 1)) +
    scale_x_discrete(name = vlab) +
    scale_y_continuous(name = "proportion", limits = c(0, 0.6)) +
    theme_light() +
    theme(
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 20)
    )
  return(f)
}

f34a <- genfig("k6nrv", "nervous")
f34b <- genfig("k6rst", "restless")
f34c <- genfig("k6dep", "depressed")
f34d <- genfig("k6hop", "hopeless")
f34e <- genfig("k6eff", "effort")
f34f <- genfig("k6wth", "worthless")

f34 <- ggarrange(f34a, f34b, f34c, f34d, f34e, f34f)
ggsave("isem-f34.pdf", f34)


### Table 3.1 -- measurement model
m1 <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
'
fit1 <- sem(m1, data = d2, meanstructure = T)
summary(fit1, rsquare = T)


### Table 3.2 -- MLEs
# standard MLE
m2 <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
  PD  ~ a*mar + fem + blk + hsp
  drk ~ b*PD + mar + fem + blk + hsp
  ie := a*b
'
fit2 <- sem(m2, data = d2)
summary(fit2, rsquare = T)

# robust MLE
fit3 <- sem(m2, data = d2, estimator = "MLR")
summary(fit3, rsquare = T)

# missing MLE
fit4 <- sem(m2, data = d1, missing = "ML")
summary(fit4, rsquare = T)


### Table 3.3 -- miiv estimators
m3 <- '
  PD =~ k6nrv + k6rst + k6dep
'
fit5 <- sem(m3, data = d2)
summary(fit5)

miivs(m3)
miive(m3, d2)

m4 <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
  PD  ~ a*mar + blk + hsp 
  drk ~ b*PD + mar + fem + blk + hsp 
'
miivs(m4)
miive(m4, d2)
