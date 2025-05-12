### Purpose: Prepare Chapter 4 examples
### Author:  S Bauldry
### Date:    Feb 4, 2025

setwd("~/desktop")
library(tidyverse)
library(lavaan)
library(ggpubr)


### Load prepared NSDUH data
d1 <- read_csv("isem-nsduh-data.csv")



### Table 4.1
m1 <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
'
fit1 <- sem(m1, data = d1)
fitMeasures(fit1, c("chisq", "df", "pvalue", "srmr", "rmsea", "cfi", "tli"))

m2 <- '
  Anx =~ k6nrv + k6rst
  Dep =~ k6dep + k6hop + k6eff + k6wth
'
fit2 <- sem(m2, data = d1)
fitMeasures(fit2, c("chisq", "df", "pvalue", "srmr", "rmsea", "cfi", "tli"))



### Table 4.2
summary(fit1, rsquare = T)
summary(fit2, rsquare = T)
standardizedSolution(fit2)



### Table 4.3
m1u <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
  k6nrv ~~ k6rst
'
fit1u <- sem(m1u, data = d1)

m1r <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
'
fit1r <- sem(m1r, data = d1)

fitMeasures(fit1u, c("chisq", "df", "pvalue", "aic", "bic"))
fitMeasures(fit1r, c("chisq", "df", "pvalue", "aic", "bic"))
anova(fit1r, fit1u)


m2u <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
  PD  ~ mar + fem + blk + hsp
  drk ~ PD + mar + fem + blk + hsp 
'
fit2u <- sem(m2u, data = d1)

m2r <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
  PD  ~ mar + fem + blk + hsp
  drk ~ PD
'
fit2r <- sem(m2r, data = d1)

fitMeasures(fit2u, c("chisq", "df", "pvalue", "aic", "bic"))
fitMeasures(fit2r, c("chisq", "df", "pvalue", "aic", "bic"))
anova(fit2r, fit2u)

fitMeasures(fit1, c("aic", "bic"))
fitMeasures(fit2, c("chisq", "aic", "bic"))



### Table 4.4
modindices(fit1, sort = T)
modindices(fit2, sort = T)

m3 <- '
  Anx =~ k6nrv + k6rst + k6eff
  Dep =~ k6dep + k6hop + k6eff + k6wth
'
fit3 <- sem(m3, data = d1)
summary(fit3, rsquare = T)
standardizedSolution(fit3)
fitMeasures(fit3, c("chisq", "df", "pvalue", "srmr", "rmsea", "cfi", "tli"))
