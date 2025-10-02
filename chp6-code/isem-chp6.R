### Purpose: Prepare Chapter 6 examples
### Author:  S Bauldry
### Date:    Mar 9, 2025

setwd("~/desktop")
library(tidyverse)
library(lavaan)
library(psych)
library(ggpubr)

d1 <- read_csv("isem-nsduh-data.csv")


### Figure 6.1 - distribution of days drinking
f61 <- ggplot(data = d1, mapping = aes(x = drk)) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  scale_x_continuous(name = "number of days consumed alcohol in past month") +
  scale_y_continuous(name = "proportion", limits = c(0, 0.5)) +
  theme_light() +
  theme(
    axis.text  = element_text(size = 18),
    axis.title = element_text(size = 20)
  )
f61
ggsave("isem-f61.pdf", f61)

prop.table( table(d1$drk) )


### Figure 6.2 - illustration of underlying continuum and thresholds
x    <- seq(-7, 7, length.out = 1000)
y    <- dnorm(x, 0, 2)
d2   <- tibble(x, y)
tau  <- c(-3.5, -1.5, 2.2, 4)
ytau <- dnorm(tau, 0, 2)
d3   <- tibble(x = tau, y = 0, yend = ytau) 

f62 <- ggplot(d2, aes(x = x, y = y)) +
  geom_line() +
  geom_segment(data = d3, aes(x = x, y = y, xend = x, yend = yend), linetype = "dashed") +
  annotate("text", x = -4.2, y = 0.006, label = "none\nof the time") +
  annotate("text", x = -2.4, y = 0.006, label = "a little\nof the time") +
  annotate("text", x =  0.5, y = 0.006, label = "some\nof the time") +
  annotate("text", x =  3.0, y = 0.006, label = "most\nof the time") +
  annotate("text", x =  4.6, y = 0.006, label = "all\nof the time") +
  scale_x_continuous(breaks = c(-6, -3.5, -1.5, 2.2, 4, 6), labels = c("never", expression(tau[1]), expression(tau[2]), expression(tau[3]), expression(tau[4]), "always")) +
  labs(x = "how often nervous in past 30 days", y = "density") +
  theme_light() +
  theme(
    axis.text  = element_text(size = 18),
    axis.title = element_text(size = 20)
  )
f62
ggsave("isem-f62.pdf", f62)


### Table 6.1 - polychoric and pearson correlations
d3 <- d1 |> 
  select(k6nrv, k6rst, k6dep, k6hop, k6eff, k6wth) |>
  drop_na()
polychoric(d3) 
cor(d3)


### Table 6.2 - measurement models model fit
m1 <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
'
fit1 <- sem(m1, data = d1, estimator = "MLR", meanstructure = T)
fit2 <- sem(m1, data = d1, ordered = c("k6nrv", "k6rst", "k6dep", "k6hop", "k6eff", "k6wth"))

m2 <- '
  Anx =~ k6nrv + k6rst 
  Dep =~ k6dep + k6hop + k6eff + k6wth
'
fit3 <- sem(m2, data = d1, estimator = "MLR", meanstructure = T)
fit4 <- sem(m2, data = d1, ordered = c("k6nrv", "k6rst", "k6dep", "k6hop", "k6eff", "k6wth"))

fitMeasures(fit1, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))
fitMeasures(fit2, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))
fitMeasures(fit3, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))
fitMeasures(fit4, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))


### Table 6.3 - measurement models parameter estimates
summary(fit1)
summary(fit2)
summary(fit3)
standardizedSolution(fit3)
summary(fit4)
standardizedSolution(fit4)


### Storing estimates for predicted probabilities and AMEs
all_tau <- lavInspect(fit2, what = "est")$tau
nrv_tau <- all_tau[1:4]
rst_tau <- all_tau[5:8]
dep_tau <- all_tau[9:12]
hop_tau <- all_tau[13:16]
eff_tau <- all_tau[17:20]
wth_tau <- all_tau[21:24]

all_lambda <- lavInspect(fit2, what = "est")$lambda
nrv_lambda <- all_lambda[1]
rst_lambda <- all_lambda[2]
dep_lambda <- all_lambda[3]
hop_lambda <- all_lambda[4]
eff_lambda <- all_lambda[5]
wth_lambda <- all_lambda[6]

PD_sd <- sqrt( lavInspect(fit2, what = "est")$psi )


### Calculating predicted probabilities for nervous indicator
pnorm( -nrv_tau )


### Figure 6.4 - predicted responses for nervous and depressed
prfig <- function(t1, t2, t3, t4, l1) {
  x <- seq(-3, 3, length.out = 500)
  p1 <- 1 - pnorm( -1*(t1) + l1*x ) 
  p2 <- pnorm( -1*(t1) + l1*x ) - pnorm( -1*(t2) + l1*x )
  p3 <- pnorm( -1*(t2) + l1*x )  - pnorm( -1*(t3) + l1*x )
  p4 <- pnorm( -1*(t3) + l1*x )  - pnorm( -1*(t4) + l1*x )
  p5 <- pnorm( -1*(t4) + l1*x )
  r = factor( rep( c("N", "L", "S", "M", "A"), each = length(x) ) )
  d <- tibble( x = rep(x, 5), p = c(p1, p2, p3, p4, p5), r = r)
  
  f <- ggplot(d, aes(x = x, y = p, group = r)) +
    geom_line() +
    labs(x = "latent psychological distress", y = "pred prob") +
    theme_light() +
    theme(
      axis.text  = element_text(size = 18),
      axis.title = element_text(size = 20)
    )
  return(f)
}


f64a <- prfig(nrv_tau[1], nrv_tau[2], nrv_tau[3], nrv_tau[4], nrv_lambda) +
  annotate("text", x = -1.5, y = 0.75, label = "N", size = 5) +
  annotate("text", x = -1.5, y = 0.20, label = "L", size = 5) +
  annotate("text", x =  0.5, y = 0.35, label = "S", size = 5) +
  annotate("text", x =  2.0, y = 0.25, label = "M", size = 5) +
  annotate("text", x =  2.0, y = 0.55, label = "A", size = 5) +
  ggtitle("Panel A: Nervous")

f64b <- prfig(dep_tau[1], dep_tau[2], dep_tau[3], dep_tau[4], dep_lambda) +
  annotate("text", x = -1.5, y = 0.93, label = "N", size = 5) +
  annotate("text", x = -1.5, y = 0.07, label = "L", size = 5) +
  annotate("text", x =  0.5, y = 0.27, label = "S", size = 5) +
  annotate("text", x =  2.0, y = 0.22, label = "M", size = 5) +
  annotate("text", x =  2.0, y = 0.55, label = "A", size = 5) +
  ggtitle("Panel B: Depressed")
f64b

f64 <- ggarrange(f64a, f64b)
ggsave("isem-f64.pdf", f64)


### Table 6.4 - marginal effects
me <- function(t1, t2, t3, t4, l1, a, b) {
  x <- c(a, b)
  p1 <- 1 - pnorm( -1*(t1) + l1*x ) 
  p2 <- pnorm( -1*(t1) + l1*x ) - pnorm( -1*(t2) + l1*x )
  p3 <- pnorm( -1*(t2) + l1*x )  - pnorm( -1*(t3) + l1*x )
  p4 <- pnorm( -1*(t3) + l1*x )  - pnorm( -1*(t4) + l1*x )
  p5 <- pnorm( -1*(t4) + l1*x )
  r = factor( rep( c("N", "L", "S", "M", "A"), each = length(x) ) )
  d <- tibble( pid = rep( c("p1", "p2"), 5), p = c(p1, p2, p3, p4, p5), r = r) |>
    pivot_wider(names_from = pid, values_from = p, id_cols = r) |>
    mutate(me = p2 - p1)
  me <- t(d[, 4])
  return(me)
}

menrv <- me(nrv_tau[1], nrv_tau[2], nrv_tau[3], nrv_tau[4], nrv_lambda, -PD_sd/2, PD_sd/2)
merst <- me(rst_tau[1], rst_tau[2], rst_tau[3], rst_tau[4], rst_lambda, -PD_sd/2, PD_sd/2)
medep <- me(dep_tau[1], dep_tau[2], dep_tau[3], dep_tau[4], dep_lambda, -PD_sd/2, PD_sd/2)
mehop <- me(hop_tau[1], hop_tau[2], hop_tau[3], hop_tau[4], hop_lambda, -PD_sd/2, PD_sd/2)
meeff <- me(eff_tau[1], eff_tau[2], eff_tau[3], eff_tau[4], eff_lambda, -PD_sd/2, PD_sd/2)
mewth <- me(wth_tau[1], wth_tau[2], wth_tau[3], wth_tau[4], wth_lambda, -PD_sd/2, PD_sd/2)

me <- rbind(menrv, merst, medep, mehop, meeff, mewth)
round(me, 2)
